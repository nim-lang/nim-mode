;;; nim-eldoc.el --- nim-mode’s eldoc library -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Yuta Yamada

;; Author: Yuta Yamada <cokesboy@gmail.com>
;; Keywords: eldoc, nim, convenience

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

;; Eldoc supports for Nim. This package automatically turns on
;; if you set ‘nim-nimsuggest-path’ and the nimsuggest is working.

;;; Code:

(require 'nim-suggest)
(require 'cl-macs)

(defvar nim-eldoc--data nil)
(defun nim-eldoc-function ()
  "Return a doc string appropriate for the current context, or nil."
  (interactive)
  (when nim-nimsuggest-path
    (unless (eq (point) (car nim-eldoc--data))
      (save-excursion
        (when (and (< 0 (nth 0 (syntax-ppss)))
                   (eq ?\( (char-after (nth 1 (syntax-ppss)))))
          (goto-char (1- (nth 1 (syntax-ppss)))))
        (nim-call-epc
         'def
         (lambda (defs)
           (let ((def (cl-first defs)))
             (when def
               (setq nim-eldoc--data
                     (list
                      (cons :str  (nim-eldoc-format-string def))
                      (cons :line (line-number-at-pos))))))))))
    (when (eq (line-number-at-pos)
              (assoc-default :line nim-eldoc--data))
      (assoc-default :str nim-eldoc--data))))

(defun nim-eldoc-format-string (data)
  "Format DATA for eldoc."
  (let* ((forth   (nim-epc-forth data))
         (symKind (nim-epc-symkind data))
         (qpath   (nim-epc-qualifiedPath data))
         (doc (mapconcat 'identity
                         (split-string (nim-epc-doc data) "\n")
                         ""))
         (name
          (if (eq (length (cdr qpath)) 1)
              (cadr qpath)
            (mapconcat 'identity (cdr qpath) "."))))
    (when name
      (add-text-properties
       0 (length name)
       '(face font-lock-function-name-face)
       name))
    (pcase (cons symKind nil)
      (`(,(or "skProc" "skField") . ,_)
       (when (string< "" forth)
         (cl-destructuring-bind (ptype . typeinfo) (nim-eldoc-parse forth)
           (when (equal "proc" ptype)
             (let* ((func  (format "%s %s" name typeinfo)))
               (nim-eldoc-trim
                (if (string= "" doc)
                    (format "%s" func)
                  (format "%s %s" func doc))))))))
      (`("skType" . ,_)
       (nim-eldoc-trim
        (if (not (string< "" doc))
            (format "there is no doc for %s" name)
          (format "%s: %s" name doc)))))))

(defun nim-eldoc-parse (forth)
  (when (string-match
         (rx (group (1+ word)) (0+ " ")
             (group (1+ nonl)))
         forth)
    (let ((first (match-string 1 forth))
          (other (match-string 2 forth)))
      (cons first other))))

(defun nim-eldoc-trim (str)
  "Adjust STR for mini buffer."
  (let ((max-width (- (frame-width) 4))) ; <- just for buffer, probably
    (if (< (length str) max-width)       ; it depends on terminal or GUI Emacs
        str
      (let* ((short-str (substring str 0 (- (frame-width) 4)))
             (minus-offset
              (cl-loop with num = 0
                       for s in (delq "" (split-string (reverse short-str) ""))
                       if (equal s ".") do (cl-return num)
                       else do (cl-incf num)
                       finally return 0)))
        (substring short-str 0 (- (length short-str) minus-offset))))))

;;;###autoload
(defun nim-eldoc-setup ()
  "Setup eldoc configuration for nim-mode."
  (when (and (eq major-mode 'nim-mode) nim-nimsuggest-path)
    (setq-local eldoc-documentation-function 'nim-eldoc-function)))

;;;###autoload
(add-hook 'nim-mode-hook 'nim-eldoc-setup)

(provide 'nim-eldoc)
;;; nim-eldoc.el ends here
