;;; nim-mode.el --- A major mode for the Nim programming language -*- lexical-binding: t -*-
;;
;; Filename: nim-mode.el
;; Description: A major mode for the Nim programming language
;; Author: Simon Hafner
;; Maintainer: Simon Hafner <hafnersimon@gmail.com>
;; Version: 0.2.0
;; Keywords: nim languages
;; Compatibility: GNU Emacs 24.4
;; Package-Requires: ((emacs "24.4") (epc "0.1.1") (let-alist "1.0.1") (commenter "0.5.1"))
;;
;; Taken over from James H. Fisher <jameshfisher@gmail.com>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Large parts of this code is shamelessly stolen from python.el and
;; adapted to Nim
;;
;; Todo:
;;
;; -- Make things non-case-sensitive and ignore underscores
;; -- Treat parameter lists separately
;; -- Treat pragmas inside "{." and ".}" separately
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'cl-lib)

;; Order of loading
(require 'nim-vars)
(require 'nim-rx)
(require 'nim-syntax)
(require 'nim-util)
(require 'nim-helper)
(require 'nim-smie)
(require 'paren) ; for ‘show-paren-data-function’
(require 'nim-fill)
(require 'nim-suggest)
(require 'nim-compile)
(require 'commenter)

(put 'nim-mode 'font-lock-defaults '(nim-font-lock-keywords nil t))

(defun nim-font-lock-syntactic-face-function (syntax-ppss)
  "Return syntactic face given SYNTAX-PPSS."
  (if (nth 4 syntax-ppss) ; if nth 4 is exist, it means inside comment.
      (if (nim-docstring-p syntax-ppss)
          font-lock-doc-face
        font-lock-comment-face)
    font-lock-string-face))

;;;###autoload
(define-derived-mode nim-mode prog-mode "Nim"
  "A major mode for the Nim programming language."
  :group 'nim
  ;; Font lock
  (setq-local font-lock-defaults
              '(nim-font-lock-keywords
                nil nil nil nil
                (font-lock-syntactic-face-function
                 . nim-font-lock-syntactic-face-function)))

  ;; Comment
  (setq-local comment-use-syntax t)
  ;; Those start and end comment variables are for initial value.
  (setq-local comment-start "#")
  (setq-local comment-end "")
  ;; actual comment values are defined here
  (setq-local commenter-config nim-comment)
  (commenter-setup)

  ;; SMIE
  (smie-setup nim-mode-smie-grammar 'nim-mode-smie-rules
              :forward-token 'nim-mode-forward-token
              :backward-token 'nim-mode-backward-token)
  (setq-local indent-line-function #'nim-indent-line)
  ;; FIXME: due to uncompleted Nim’s smie grammar,
  ;; ‘smie--matching-block-data’ function gets stop when
  ;; the cursor is at proc/template/macro to find terminator
  ;; (I guess). To prevent this, temporary use default
  ;; show-paren-mode’s function instead.
  (setq-local show-paren-data-function #'show-paren--default)

  ;; Always indent with SPACES!
  (setq-local indent-tabs-mode nil)
  (setq-local parse-sexp-lookup-properties t)
  (setq-local parse-sexp-ignore-comments t)

  ;; Syntax highlight for strings
  (setq-local syntax-propertize-function nim-syntax-propertize-function)

  ;; Paragraph
  (setq-local paragraph-start "\\s-*$")
  ;; Navigation
  (setq-local beginning-of-defun-function #'nim-nav-beginning-of-defun) ; C-M-a
  (setq-local end-of-defun-function       #'nim-nav-end-of-defun)       ; C-M-e
  ;; Fill
  (setq-local fill-paragraph-function     #'nim-fill-paragraph)
  ;; add-log
  (setq-local add-log-current-defun-function #'nim-info-current-defun)

  ;; Hooks
  ;; Add """ ... """ pairing to electric-pair-mode.
  (add-hook 'post-self-insert-hook
            #'nim-electric-pair-string-delimiter 'append t)
  (add-hook 'post-self-insert-hook
            #'nim-indent-post-self-insert-function 'append 'local)
  (add-hook 'which-func-functions #'nim-info-current-defun nil t)

  ;; Because indentation is not redundant, we cannot safely reindent code.
  (setq-local electric-indent-inhibit t)
  (setq-local electric-indent-chars (cons ?: electric-indent-chars)))

;; add ‘nim-indent-function’ to electric-indent’s
;; blocklist. ‘electric-indent-inhibit’ isn’t enough for old emacs.
(add-to-list 'electric-indent-functions-without-reindent 'nim-indent-line)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.nim\\(ble\\|s\\)?\\'" . nim-mode))

(defun nim-indent-post-self-insert-function ()
  "Adjust indentation after insertion of some characters.
This function is intended to be added to `post-self-insert-hook.'
If a line renders a paren alone, after adding a char before it,
the line will be re-indented automatically if needed."
  (when (and electric-indent-mode
             (eq (char-before) last-command-event))
    (cond
     ;; Electric indent inside parens
     ((and
       (not (bolp))
       (let ((paren-start (nim-syntax-context 'paren)))
         ;; Check that point is inside parens.
         (when paren-start
           (not
            ;; Filter the case where input is happening in the same
            ;; line where the open paren is.
            (= (line-number-at-pos)
               (line-number-at-pos paren-start)))))
       ;; When content has been added before the closing paren or a
       ;; comma has been inserted, it's ok to do the trick.
       (or
        (memq (char-after) '(?\) ?\] ?\}))
        (eq (char-before) ?,)))
      (save-excursion
        (goto-char (line-beginning-position))
        (let ((indentation (nim-indent-calculate-indentation)))
          (when (and (numberp indentation) (< (current-indentation) indentation))
            (indent-line-to indentation)))))
     ;; Electric colon
     ((and (eq ?: last-command-event)
           (memq ?: electric-indent-chars)
           (not current-prefix-arg)
           ;; Trigger electric colon only at end of line
           (eolp)
           ;; Avoid re-indenting on extra colon
           (not (equal ?: (char-before (1- (point)))))
           (not (nim-syntax-comment-or-string-p)))
      ;; Just re-indent dedenters
      (let ((dedenter-pos (nim-info-dedenter-statement-p))
            (current-pos (point)))
        (when dedenter-pos
          (save-excursion
            (goto-char dedenter-pos)
            (nim--indent-line-core)
            (unless (= (line-number-at-pos dedenter-pos)
                       (line-number-at-pos current-pos))
              ;; Reindent region if this is a multiline statement
              (indent-region dedenter-pos current-pos)))))))))

(defun nim-indent-electric-colon (arg)
  "Insert a colon and maybe de-indent the current line.
With numeric ARG, just insert that many colons.  With
\\[universal-argument], just insert a single colon."
  (interactive "*P")
  (self-insert-command (if (not (integerp arg)) 1 arg))
  (when (and (not arg)
             (eolp)
             (not (equal ?: (char-after (- (point-marker) 2))))
             (not (nim-syntax-comment-or-string-p)))
    (let ((indentation (current-indentation))
          (calculated-indentation (nim-indent-calculate-indentation)))
      (when (> indentation calculated-indentation)
        (save-excursion
          (indent-line-to calculated-indentation))))))
(put 'nim-indent-electric-colon 'delete-selection t)

;; hideshow.el (hs-minor-mode)
(defun nim-hideshow-forward-sexp-function (_arg)
  "Nim specific `forward-sexp' function for `hs-minor-mode'.
Argument ARG is ignored."
  (nim-nav-end-of-defun)
  (unless (nim-line-empty-p)
    (backward-char)))

(add-to-list
 'hs-special-modes-alist
 `(nim-mode
   ,nim-nav-beginning-of-defun-regexp
   ;; Use the empty string as end regexp so it doesn't default to
   ;; "\\s)".  This way parens at end of defun are properly hidden.
   ""
   "#"
   nim-hideshow-forward-sexp-function
   nil))

(provide 'nim-mode)

;;; nim-mode.el ends here
