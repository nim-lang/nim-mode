;;; company-nim.el --- company backend for nim -*- lexical-binding: t -*-

;; Copyright (C) 2015  Simon Hafner

;; Author: Simon Hafner
;; Maintainer: Simon Hafner <hafnersimon@gmail.com>
;; Package-Version: 0.2.0
;; Package-Requires: ((company "0.8.10") (nim-mode "0.2.0"))
;; Keywords: convenience

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

;; It contains company backend with nimsuggest support.
;; You have to add it to company-backends like:
;;
;; (add-to-list 'company-backends
;;                '(company-nim :with company-nim-builtin))
;;
;; Also you should add company mode to nim-mode
;;
;; (add-hook 'nim-mode-hook 'company-mode)


;;; Code:

(require 'epc)
(require 'nim-mode)
(require 'company)
(require 'cl-lib)

(defcustom company-nim-type-abbrevs '(
                                 ("skProc" . "f")
                                 ("skIterator" . "i")
                                 ("skTemplate" . "T")
                                 ("skType" . "t")
                                 ("skMethod" . "f")
                                 ("skEnumField" . "e")
                                 ("skGenericParam" . "p")
                                 ("skParam" . "p")
                                 ("skModule" . "m")
                                 ("skConverter" . "C")
                                 ("skMacro" . "M")
                                 ("skField" . "F")
                                 ("skForVar" . "v")
                                 ("skVar" . "v")
                                 ("skLet" . "v")
                                 ("skLabel" . "l")
                                 ("skConst" . "c")
                                 ("skResult" . "r")
                                 )
  "Abbrevs for nim-mode (used by company)"
  :type 'assoc
  :group 'nim)


(defun company-nim--format-candidate (cand)
  "Formats candidate for company, attaches properties to text."
  (propertize
   (car (last (nim-epc-qualifiedPath cand)))
   :nim-location-line (nim-epc-line cand)
   :nim-location-column (nim-epc-column cand)
   :nim-type (nim-epc-forth cand)
   :nim-doc (nim-epc-doc cand)
   :nim-file (nim-epc-filePath cand)
   :nim-sk (nim-epc-symkind cand)
   :nim-sig (assoc-default (nim-epc-symkind cand) company-nim-type-abbrevs)))

(defun company-nim--format-candidates (arg candidates)
  "Filters candidates, and returns formatted candadates lists."
  (mapcar #'company-nim--format-candidate
          (if (string-equal arg ".")
              candidates
            (cl-remove-if-not
             (lambda (c)
               (company-nim-fuzzy-match
                arg (car (last (nim-epc-qualifiedPath c)))))
             candidates))))


(defun company-nim-candidates (arg callback)
  (when (derived-mode-p 'nim-mode)
    (nim-call-epc
     'sug
     (lambda (x) (funcall callback (company-nim--format-candidates arg x))))))


(defun company-nim-prefix (&optional use-dotty-syntax)
  "checks if company-nim can complete here"
  (when (derived-mode-p 'nim-mode)
    (let ((thing 'stop))
      (and (not (company-in-string-or-comment))
           (setq thing
                 (substring-no-properties
                  (if use-dotty-syntax
                      ;; grab dot included symbol like XXX.YYY
                      (with-syntax-table nim-dotty-syntax-table
                        (company-grab-symbol))
                    (company-grab-symbol))))
           (cons thing t)))))


(defun company-nim-annotation (cand)
  (let ((ann (get-text-property 0 :nim-type cand))
        (symbol (get-text-property 0 :nim-sig cand)))
    (format " %s [%s]" (substring ann 0 (cl-search "{" ann)) symbol)))

;; :nim-type is frequently way too big to display in meta
;; (defun company-nim-meta (cand)
;;   (let ((doc (get-text-property 0 :nim-doc cand)))
;;     (if (eq doc "")
;;         (get-text-property 0 :nim-type cand)
;;       doc)))

(defun company-nim-meta (cand)
  (get-text-property 0 :nim-type cand))


(defun company-nim-doc-buffer (cand)
  (let ((doc
         (get-text-property 0 :nim-doc cand)))
    (and (not (eq doc "")) (nim-doc-buffer cand))))

(defun company-nim-location (cand)
  (let ((line (get-text-property 0 :nim-location-line cand))
        (path (get-text-property 0 :nim-file cand)))
    (cons path line)))


(defun company-nim-fuzzy-match (prefix candidate)
  "Basic fuzzy match for completions."
  (cl-subsetp (string-to-list prefix)
              (string-to-list candidate)))

;;;###autoload
(defun company-nim (command &optional arg &rest ignored)
  "`company-mode` backend for nimsuggest."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-nim))
    (prefix (company-nim-prefix))
    (annotation (company-nim-annotation arg))
    (doc-buffer (company-nim-doc-buffer arg))
    (meta (company-nim-meta arg))
    (location (company-nim-location arg))
    (candidates (cons :async (lambda (cb) (company-nim-candidates arg cb))))
    (ignore-case t)
    (sorted t)))

(defun company-nim-builtin-prefix (arg)
  (let ((prefix (company-nim-prefix t))
        (thing (or arg "")))
    (and
     ;; ignore auto-completion when point is empty string
     ;; (but you can activate manually)
     (or this-command (string< "" thing))
     (or (equal "" thing) (not (string-match "\\." thing)))
     prefix)))

;;;###autoload
(defun company-nim-builtin (command &optional arg &rest ignored)
  "`company-mode` backend for Nim’s primitive functions."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-nim-builtin))
    (prefix (company-nim-builtin-prefix arg))
    (annotation (company-nim-annotation arg))
    (doc-buffer (company-nim-doc-buffer arg))
    (meta (company-nim-meta arg))
    (location (company-nim-location arg))
    ;; TODO: use company-flx (fuzzy match library)
    (candidates
     (all-completions
      arg
      (company-nim--format-candidates
       arg (company-nim-get-builtin-candidates))
      (lambda (candidate)
        ;; Only first char is case sensitive in Nim and
        ;; I think it’s convenience to distinguish between normal
        ;; procs and types. (types often use capitalized character, so)
        (if (string< "" arg)
            (eq (string-to-char arg) (string-to-char candidate))
          t))))
    (ignore-case t)
    (sorted t)))

(defvar company-nim--builtin-cache nil)
(defvar company-nimscript--builtin-cache nil)

(defun company-nim-get-options ()
  (append nim-suggest-options nim-suggest-local-options))

(defun company-nim-get-builtin-candidates ()
  "Get candidates based on system.nim."
  (when (derived-mode-p 'nim-mode)
    (let ((cache-sym (cl-case major-mode
                       (nim-mode 'company-nim--builtin-cache)
                       (nimscript-mode 'company-nimscript--builtin-cache))))
      (or
       ;; Use cached data, which corresponds to ‘company-nim-get-options’
       (cl-loop for (option suggest) in (symbol-value cache-sym)
                if (equal (cdr option) (company-nim-get-options))
                do (cl-return (cdr suggest)))
       ;; Save data asynchronously
       (save-excursion
         (add-to-list cache-sym
                      (list (cons 'option (company-nim-get-options))
                            (cons 'suggest nil)))
         (insert "\n system.")
         (nim-call-epc
          'sug
          `(lambda (suggests)
             (cl-loop for i from 0 to (1- (length ,cache-sym))
                      for c = (nth i ,cache-sym)
                      if (and (equal (cdar c) (company-nim-get-options))
                              (eq (cdadr c) nil))
                      do (setf (cdadr (nth i ,cache-sym))
                               suggests))))
         (delete-char -8))))))

(provide 'company-nim)

;;; company-nim.el ends here
