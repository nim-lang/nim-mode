;;; nim-mode.el --- A major mode for the Nim programming language -*- lexical-binding: t -*-
;;
;; Filename: nim-mode.el
;; Description: A major mode for the Nim programming language
;; Author: Simon Hafner
;; Maintainer: Simon Hafner <hafnersimon@gmail.com>
;; Version: 0.2.0
;; Keywords: nim languages
;; Compatibility: GNU Emacs 24.4
;; Package-Requires: ((emacs "24.4") (epc "0.1.1"))
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
;; -- Identifier following "proc" gets font-lock-function-name-face
;; -- Treat parameter lists separately
;; -- Treat pragmas inside "{." and ".}" separately
;; -- Highlight tabs as syntax error
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'cl-lib)

;; Order of loading
(require 'nim-vars)
(eval-and-compile (require 'nim-rx))
(require 'nim-syntax)
(require 'nim-util)
(require 'nim-helper)
(require 'nim-indent)
(require 'nim-fill)
(require 'nim-suggest)
(require 'nim-compile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Helpers                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nim-glue-strings (glue strings)
  "Concatenate some GLUE and a list of STRINGS."
  (mapconcat 'identity strings glue))

(defun nim-regexp-choice (strings)
  "Construct a regexp multiple-choice from a list of STRINGS."
  (concat "\\(" (nim-glue-strings "\\|" strings) "\\)"))

(put 'nim-mode 'font-lock-defaults '(nim-font-lock-keywords nil t))

;;;###autoload
(define-derived-mode nim-mode prog-mode "Nim"
  "A major mode for the Nim programming language."
  :group 'nim
  ;; Font lock
  (set (make-local-variable 'font-lock-defaults)
       '(nim-font-lock-keywords
         nil nil nil nil
         (font-lock-syntactic-face-function
          . nim-font-lock-syntactic-face-function)))
  ;; Add """ ... """ pairing to electric-pair-mode.
  (add-hook 'post-self-insert-hook
            #'nim-electric-pair-string-delimiter 'append t)
  ;; Comment
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#+\\s-*")

  ;; Indent
  (with-no-warnings
    (if (and nim-use-smie-indent)
        ;; SMIE
        (progn
          (require 'nim-smie nil t)
          (smie-setup nim-mode-smie-grammar 'nim-mode-smie-rules
                      :forward-token 'nim-mode-forward-token
                      :backward-token 'nim-mode-backward-token)
          (set (make-local-variable 'indent-line-function)
               'nim-smie-indent-line-function)
          ;; FIXME: due to uncompleted Nim’s smie grammar,
          ;; ‘smie--matching-block-data’ function gets stop when
          ;; the cursor is at proc/template/macro to find terminator
          ;; (I guess). To prevent this, temporary use default
          ;; show-paren-mode’s function instead.
          (set (make-local-variable 'show-paren-data-function)
               #'show-paren--default))
      ;; Old indentation bindings
      (set (make-local-variable 'indent-line-function) #'nim-indent-line-function)
      (set (make-local-variable 'indent-region-function) #'nim-indent-region)
      (set (make-local-variable 'forward-sexp-function)
           'nim-nav-forward-sexp)
      (set (make-local-variable 'fill-paragraph-function) #'nim-fill-paragraph)))

  ;; Always indent with SPACES!
  (set (make-local-variable 'indent-tabs-mode) nil)

  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)

  ;; Syntax highlight for strings
  (set (make-local-variable 'syntax-propertize-function)
       nim-syntax-propertize-function)

  ;; Because indentation is not redundant, we cannot safely reindent code.
  (set (make-local-variable 'electric-indent-inhibit) t)
  (set (make-local-variable 'electric-indent-chars)
       (cons ?: electric-indent-chars))
  ;; Paragraph
  (set (make-local-variable 'paragraph-start) "\\s-*$")

  ;; Navigation
  (set (make-local-variable 'beginning-of-defun-function)
       #'nim-nav-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       #'nim-nav-end-of-defun)
  (set (make-local-variable 'add-log-current-defun-function)
       #'nim-info-current-defun)
  (add-hook 'post-self-insert-hook
            #'nim-indent-post-self-insert-function 'append 'local)
  (add-hook 'which-func-functions #'nim-info-current-defun nil t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.nim\\'" . nim-mode))

(provide 'nim-mode)

;;; nim-mode.el ends here
