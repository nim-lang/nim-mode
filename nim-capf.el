;;; nim-capf.el --- Implementation of Completion At Point Function for nim-mode -*- lexical-binding: t -*-

;; Copyright (C) 2016  Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; Keywords: completion

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides Completion At Point Function future (capf in
;; short) and you can use it via C-M-i and M-TAB keys by Emacs'
;; default key bindings.
;;
;; Also capf allows you to work with company-mode without adding
;; backends.

;; For company-mode users:
;; You can configure minimum string length that company's auto-completion
;; starts by `company-minimum-prefix-length'.  Also you can change the idle
;; time by `company-idle-delay'.

;; TODO:
;;   - make sure with company-flx package (https://github.com/PythonNut/company-flx)
;;     (currently somehow I couldn't use it)
;;
;;; Code:

(require 'let-alist)
(require 'nim-syntax)
(require 'nim-suggest)
(require 'nim-helper)

(defcustom nim-capf--type-abbrevs '(("skProc"         . "f")
                                    ("skIterator"     . "i")
                                    ("skTemplate"     . "T")
                                    ("skType"         . "t")
                                    ("skMethod"       . "f")
                                    ("skEnumField"    . "e")
                                    ("skGenericParam" . "p")
                                    ("skParam"        . "p")
                                    ("skModule"       . "m")
                                    ("skConverter"    . "C")
                                    ("skMacro"        . "M")
                                    ("skField"        . "F")
                                    ("skForVar"       . "v")
                                    ("skVar"          . "v")
                                    ("skLet"          . "v")
                                    ("skLabel"        . "l")
                                    ("skConst"        . "c")
                                    ("skResult"       . "r"))
  "Abbrevs for completion."
  :type 'assoc
  :group 'nim)

(defun nim-capf--format-candidate (cand)
  "Put text property to CAND."
  (let ((qpath (nim-epc-qualifiedPath cand)))
    (propertize
     (car (last qpath))
     :nim-line   (nim-epc-line     cand)
     :nim-column (nim-epc-column   cand)
     :nim-type   (nim-epc-forth    cand)
     :nim-doc    (nim-epc-doc      cand)
     :nim-qpath  qpath
     :nim-file   (nim-epc-filePath cand)
     :nim-sk     (nim-epc-symkind  cand)
     :nim-sig    (assoc-default
                  (nim-epc-symkind cand) nim-capf--type-abbrevs))))

(defun nim-capf--format-candidates (_arg candidates)
  "Put text attributes to CANDIDATES."
  (mapcar #'nim-capf--format-candidate candidates))

(defun nim-capf--nimsuggest-async (prefix callback)
  "Query to nimsuggest asynchronously.

The PREFIX is passed to the CALLBACK."
  ;; currently only support nim-mode (not nimscript-mode)
  (when (derived-mode-p 'nim-mode)
    (nim-call-epc
     'sug
     (lambda (x) (funcall callback (nim-capf--format-candidates prefix x))))))

(defun nim-capf--prefix-p (beg end &optional skip)
  "Return t if completion should be triggered for prefix between BEG and END.
If SKIP is non-nil, skip length check ."
  (and
   (if (or skip (eq this-command 'company-idle-begin)
           (eq ?. (char-before beg)))
       t
     (let ((diff (- end beg))
           (len (bound-and-true-p company-minimum-prefix-length)))
       (and len (<= len diff))))
   (<= beg end)
   (or (eolp)
       (let ((c-end (char-after end)))
         (and c-end (not (eq ?w (char-syntax c-end))))))))

(defun nim-capf--annotation (cand)
  "Get annotation info for CAND."
  (let ((ann (get-text-property 0 :nim-type cand))
        (symbol (get-text-property 0 :nim-sig cand)))
    (format " %s [%s]" (substring ann 0 (cl-search "{" ann)) symbol)))

(defun nim-capf--docsig (cand)
  "Get docsig info for CAND."
  (format "%s %s"
          (car (get-text-property 0 :nim-qpath cand))
          (get-text-property 0 :nim-type cand)))

(defun nim-capf--doc-buffer (cand)
  "Get doc-buffer info for CAND."
  (let ((doc (get-text-property 0 :nim-doc cand)))
    (unless (equal doc "")
      (nim-capf--doc-buffer-core cand))))

(defun nim-capf--doc-buffer-core (element)
  "Displays documentation buffer with ELEMENT contents."
  (let ((buf (get-buffer-create "*nim-doc*")))
    (with-current-buffer buf
      (view-mode -1)
      (erase-buffer)
      (insert (get-text-property 0 :nim-doc element))
      (goto-char (point-min))
      (view-mode 1)
      buf)))

(defun nim-capf--location (cand)
  "Get location info for CAND."
  (let ((line (get-text-property 0 :nim-line cand))
        (path (get-text-property 0 :nim-file cand)))
    (cons path line)))

;;;###autoload
(defun nim-capf-nimsuggest-completion-at-point ()
  "Complete the symbol at point using nimsuggest."
  (when nimsuggest-mode
    (unless (nth 3 (syntax-ppss)) ;; not in string
      (let* ((bounds (bounds-of-thing-at-point 'symbol))
             (beg (or (car bounds) (point)))
             (end (or (cdr bounds) (point)))
             (c-beg (char-after beg))
             ;; avoid length check if previous char is "."
             (skip-len-check (and (not (bobp)) (eq ?. (char-before (point))))))
        (list beg end
              (completion-table-with-cache 'nim-capf--nimsuggest-complete)
              ;; See `completion-extra-properties' for details
              :exit-function #'nim-capf--exit-function    ; replacement of company's :post-completion
              :annotation-function #'nim-capf--annotation ; show annotation right after completion
              ;; default property of ‘completion-at-point-functions’
              :exclusive 'no
              :predicate `(lambda (candidate)
                            (if ,(and c-beg (< 65 c-beg 90)) ; whether A-Z
                                (let ((thing (thing-at-point 'symbol)))
                                  (if thing
                                      ;; If user inputs capitalized string,
                                      ;; check only the first char.
                                      (eq ,c-beg (string-to-char candidate))
                                    ;; let default predicate function
                                    t))
                              t))
              ;; Company-mode integration
              ;; predicate by length
              :company-prefix-length (nim-capf--prefix-p beg end skip-len-check)
              ;; show something on minibuffer
              :company-docsig #'nim-capf--docsig
              ;; you can activate via F1 key, but currently no documentation available.
              :company-doc-buffer #'nim-capf--doc-buffer
              ;; C-w key to open the source location
              :company-location #'nim-capf--location)))))

(defun nim-capf--nimsuggest-complete (prefix)
  "Completion symbol of PREFIX at point using nimsuggest."
  (unless (or (nim-inside-pragma-p)
              (nim-syntax-comment-or-string-p))
    (cond
     ((or (string< "" prefix)
          (eq ?. (char-before (point))))
      (nim-capf--update prefix)))))

(defun nim-capf--update (prefix)
  "Query completion to nimsuggest.
PREFIX is passed to async callback."
  (let* ((buf (current-buffer))
         (start (time-to-seconds))
         (res 'trash))
    (nim-capf--nimsuggest-async
     prefix
     (lambda (candidates)
       (when (eq (current-buffer) buf)
         (setq res candidates))))
    (while (and (eq 'trash res) (eq (current-buffer) buf))
      (if (> (- (time-to-seconds) start) 2)
          (error "Nimsuggest completion: timeout %d sec" 2)
        (sleep-for 0.03)))
    (unless (eq 'trash res)
      res)))

(defun nim-capf--exit-function (str status)
  "Insert necessary things for STR, when completion is done.
You may see information about STATUS at `completion-extra-properties'.
But, for some reason, currently this future is only supporting
company-mode.  See also: https://github.com/company-mode/company-mode/issues/583"
  (unless (eq 'completion-at-point this-command)
    (cl-case status
      ;; finished -- completion was finished and there is no other completion
      ;; sole -- completion was finished and there is/are other completion(s)
      ((finished sole)
       (when-let ((type-sig (get-text-property 0 :nim-sig str)))
         (cl-case (intern type-sig)
           ((f T)
            (insert "()")
            (backward-char 1)
            (run-hook-with-args 'nim-capf-after-exit-function-hook str)))))
      (t
       ;; let other completion backends
       (setq this-command 'self-insert-command)))))

;; completion at point
(defun nim-capf-builtin-completion ()
  "This might not be precise, but maybe enough to someone."
  (append nim-keywords
          nim-types
          nim-exceptions
          nim-variables
          nim-constants
          nim-nonoverloadable-builtins
          nim-builtins))

(defconst nim-capf-builtin-words
  (append (nim-capf-builtin-completion)
          nim-builtins-without-nimscript))

(defconst nim-capf-builtin-words-nimscript
  (append (nim-capf-builtin-completion)
          (append nimscript-builtins
                  nimscript-variables)))

(defvar nim-capf--pragma-words
  (cl-loop for (kwd . _) in nim-pragmas collect kwd)
  "List of pragmas for `complietion-at-point-functions'.")

(defun nim-capf--static-completion (words)
  "Return list of completion-at-point’s elements.
List of WORDS are used as completion candidates."
  (unless (nth 3 (syntax-ppss)) ;; not in string
    (when (or this-command (thing-at-point 'symbol))
      (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (beg (or (car bounds) (point)))
           (end (or (cdr bounds) (point))))
      (list beg end words
            :company-prefix-length (nim-capf--prefix-p beg end)
            :exclusive 'no)))))

;;;###autoload
(defun nim-builtin-completion-at-point ()
  "Complete the symbol at point for .nim files."
  (nim-capf--static-completion
   (if (nim-inside-pragma-p)
       nim-capf--pragma-words
     nim-capf-builtin-words)))

;;;###autoload
(defun nimscript-builtin-completion-at-point ()
  "Complete the symbol at point for nimscript files."
  (nim-capf--static-completion nim-capf-builtin-words-nimscript))

;;;###autoload
(defun nim-capf-setup ()
  "Setup."
  (let ((capf (cl-case major-mode
                (nim-mode       'nim-builtin-completion-at-point)
                (nimscript-mode 'nimscript-builtin-completion-at-point)
                (t (error "Unexpected major mode")))))
    ;; Don’t change order here
    (unless (memq capf completion-at-point-functions)
      (add-hook 'completion-at-point-functions capf))
    (unless (memq 'nim-capf-nimsuggest-completion-at-point completion-at-point-functions)
      (add-hook 'completion-at-point-functions 'nim-capf-nimsuggest-completion-at-point))))

(provide 'nim-capf)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; nim-capf.el ends here
