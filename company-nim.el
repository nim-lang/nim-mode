;;; company-nim.el --- company backend for nim

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
;; (add-to-list 'company-backends 'company-nim)
;;
;; Also you should add company mode to nim-mode
;;
;; (add-hook 'nim-mode-hook 'company-mode)


;;; Code:

(require 'epc)
(require 'nim-mode)
(require 'company)

(defun company-nim--format-candidate (cand)
  "Formats candidate for company, attaches properties to text."
  (propertize (car (last (nim-epc-qualifiedPath cand)))
              :nim-location-line (nim-epc-line cand)
              :nim-location-column (nim-epc-column cand)
              :nim-type (nim-epc-forth cand)
              :nim-doc (nim-epc-doc cand)
              :nim-file (nim-epc-filePath cand)
              :nim-sk (nim-epc-symkind cand))
  )

(defun company-nim--format-candidates (arg candidates)
  "Filters candidates, and returns formatted candadates lists."
  (mapcar #'company-nim--format-candidate
          (remove-if-not
           (lambda (c) (company-nim-fuzzy-match arg (car (last (nim-epc-qualifiedPath c)))))
           candidates)))


(defun company-nim-candidates (arg callback)
  (when (derived-mode-p 'nim-mode)
    (lexical-let ((cb callback)
                  (local-arg arg))
      (nim-call-epc 'sug (lambda (x) (funcall cb (company-nim--format-candidates local-arg x)))))))


(defun company-nim-prefix ()
  (when (derived-mode-p 'nim-mode)
    (company-grab-symbol)))


(defun company-nim-annotation (cand)
  (format " %s"
          (get-text-property 0 :nim-type cand)
          ;; (get-text-property 0 :nim-type cand)
          ))

(defun company-nim-meta (cand)
  (let ((doc (get-text-property 0 :nim-doc cand)))
    (if (eq doc "")
        (get-text-property 0 :nim-type cand)
      doc)))


(defun company-nim-doc-buffer (cand)
  (get-text-property 0 :nim-doc cand))

(defun company-nim-location (cand)
  (let ((line (get-text-property 0 :nim-location-line cand))
        (path (get-text-property 0 :nim-file cand)))
    (cons path line)))


(defun company-nim-fuzzy-match (prefix candidate)
  "Basic fuzzy match for completions."
  (cl-subsetp (string-to-list prefix)
              (string-to-list candidate)))

(defun company-nim (command &optional arg &rest ignored)
  "`company-mode` backend for nimsuggest."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-nim))
    (prefix (company-nim-prefix))
    (annotation (company-nim-annotation arg))
    (doc-buffer (company-nim-doc-buffer arg))
    (meta (company-nim-meta arg))
    (candidates (lexical-let ((local-arg arg))
                  (cons :async (lambda (cb) (company-nim-candidates local-arg cb)))))
    (ignore-case t)
    (sorted t)
    ))

(provide 'company-nim)

;;; company-nim.el ends here
