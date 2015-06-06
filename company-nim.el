;;; company-nim.el --- company source for nim

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

;; Provides an auto-complete source for nim using the "nim" executable.
;; To enable this auto-complete source in nim-mode, use code like the
;; following:
;;
;; (eval-after-load 'nim-mode
;;   '(add-hook 'nim-mode-hook 'ac-nim-enable))
;;

;;; Code:

(require 'nim-mode)
(require 'epc)
(require 'company)

(defun nim-company-attach-as-text-property (epc-returns)
  "Converts nim-epc to string with nim-epc attached."
  (mapcar (lambda (epc-value)
            (let ((s (car (last (nim-epc-qualifiedPath epc-value)))))
              (put-text-property 0 (length s) 'nim-epc epc-value s)
              s))))

(defun nim-company-format-location (text)
  "Grabs the text-property and returns the cons for company."
  (let ((nim-epc (get-text-property 0 'nim-epc text)))
    ((nim-epc-filePath nim-epc) . (nim-epc-line nim-epc))))

(defun nim-company-format-def (definitions)
  (let ((definition (first definitions)))
    (cons (nim-epc-filePath definition) (nim-epc-line definition))))

;;;###autoload
(defun company-nim (command &optional arg &rest ignored)
  "`company-mode' completion backend for nim."
  (interactive (list 'iteractive))
  (cl-case command
    (interactive (company-begin-backend 'company-nim))
    (init (when (not nim-nimsuggest-path) (error "company-nim requires nim-nimsuggest-path to be set")))
    (prefix (derived-mode-p 'nim-mode))
    (candidates (nim-company-attach-as-text-property (nim-call-epc 'sug)))
    (duplicates nil)
    ;; (annotation)
    (location (nim-company-format-location arg))))

(provide 'company-nim)
