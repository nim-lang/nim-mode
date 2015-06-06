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
;; then you have to start server like:
;;
;; M-x nim:start-nimsuggest

;;; Code:

(require 'epc)


(defcustom nim:nimsuggest-path "nimsuggest"
  "Path where nimsuggest is set it to valid.")

(defvar nim:epc nil)
(make-variable-buffer-local 'nim:epc)

(defvar nim:nimsuggest-main-file nil
  "Used to keep project file available to functions.")
(make-variable-buffer-local 'nim:nimsuggest-main-file)

(defun nim:start-nimsuggest ()
  "Starts nimsuggest epc server, asks for project file."
  ;; TODO: handle multiple servers
  (interactive)
  (setq nim:nimsuggest-main-file (read-file-name "Project file: "))
  (setq nim:epc (epc:start-epc
                 nim:nimsuggest-path
                 (list "--epc" nim:nimsuggest-main-file))))


(defun nim:stop-nimsuggest ()
  "Stops previously started nimsuggest epc server"
  (interactive)
  (epc:stop-epc nim:epc)
  (setq nim:epc nil))


(defun nim:restart-nimsuggest ()
  "Restarts  previously started nimsuggest epc server."
  (interactive)
  (epc:stop-epc nim:epc)
  (setq nim:epc (epc:start-epc
                 nim:nimsuggest-path
                 (list "--epc" nim:nimsuggest-main-file))))

(defun company-nim--format-candidate (cand)
  "Formats candidate for company, attaches properties to text."
  ; FIXME: looks strange
  (propertize (car (last (nth 2 cand)))
              :nim-location-line (nth 5 cand)
              :nim-location-column (nth 6 cand)
              :nim-type (nth 4 cand)
              :nim-doc (nth 7 cand)
              :nim-file (nth 2 cand)
              :nim-sk (nth 1 cand))
  )

(defun company-nim--format-candidates (arg candidates)
  "Filters candidates, and returns formatted candadates lists."
  (mapcar #'company-nim--format-candidate
          (remove-if-not
           (lambda (c) (company-nim-fuzzy-match arg (car (last (nth 2 c)))))
           candidates)))


;; (defun company-nim-candidates (callback)
;;   (message "starting")
;;   (when (eq major-mode 'nim-mode)
;;     (deferred:$
;;       (epc:call-deferred
;;        nim
;;        'sug
;;        (list (buffer-file-name)
;;              (line-number-at-pos)
;;              (current-column)
;;              (nim-save-buffer-temporarly)))
;;       (deferred:nextc it
;;         (lambda (x) (company-nim--format-candidates x))
;;         )
;;       (deferred:nextc it
;;         ; FIXME: for some reason callback isn't the right callback there
;;         (lambda (x) (funcall callback x))
;;         )
;;       )))


(defun company-nim-candidates-sync (arg)
  "Calls epc server in sync mode."
  (when (derived-mode-p 'nim-mode)
    (company-nim--format-candidates arg (epc:call-sync
                                     nim:epc
                                     'sug
                                     (list (buffer-file-name)
                                           (line-number-at-pos)
                                           (current-column)
                                           (nim-save-buffer-temporarly))))
    ))


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
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-nim))
    (prefix (company-nim-prefix))
    (candidates (company-nim-candidates-sync arg))
    (annotation (company-nim-annotation arg))
    (doc-buffer (company-nim-doc-buffer arg))
    (meta (company-nim-meta arg))
    (ignore-case t)
    (sorted t)
    ;; (candidates (cons :async #'company-nim-candidates))
  ))

(provide 'company-nim)

;;; company-nim.el ends here
