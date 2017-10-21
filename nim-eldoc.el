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

;; Eldoc supports for Nim.  This package automatically turns on.

;;; Code:

(require 'nim-vars)
(require 'nim-helper)
(require 'cl-lib)
(require 'eldoc)

(defvar nim-eldoc--skip-regex
  (rx (or (group symbol-start
                 (or "if" "when" "elif" "while"
                     ;; for tuple assignment
                     "var" "let" "const")
                 symbol-end (0+ " "))
          (group line-start (0+ " ")))))

(defun nim-eldoc-p()
  "Return non-nil if `eldoc-mode' or `global-eldoc-mode' were non-nil."
  (or (bound-and-true-p eldoc-mode)
      ;; This mode was added at Emacs 25
      (bound-and-true-p global-eldoc-mode)))

;;;###autoload
(defun nim-eldoc-function ()
  "Return a doc string appropriate for the current context, or nil."
  (interactive)
  (when (and (nim-eldoc-p)
             (not (eq ?\s (char-after (point)))))
    (if (nim-inside-pragma-p)
        (nim-eldoc--pragma-at-point)
      (funcall 'nimsuggest-eldoc--nimsuggest))))

;;;###autoload
(defun nim-eldoc-on ()
  "This may or may not work.  Maybe this configuration has to set on.
Major-mode configuration according to the document."
  (interactive)
  (add-function :before-until (local 'eldoc-documentation-function)
                'nim-eldoc-function))

(defun nim-eldoc-off ()
  (interactive)
  (remove-function (local 'eldoc-documentation-function) 'nim-eldoc-function))

;;;###autoload
(defun nim-eldoc-setup ()
  (if (nim-eldoc-p) (nim-eldoc-on) (nim-eldoc-off)))

(defun nim-eldoc--get-pragma (pragma)
  "Get the PRAGMA's doc string."
  (let ((data (assoc-default pragma nim-pragmas)))
    (cl-typecase data
      (string data)
      ;; FIXME: more better operation
      (list (car data)))))

(defun nim-eldoc--pragma-at-point ()
  "Return string of pragma's description at point."
  (let* ((thing (thing-at-point 'symbol))
         (desc (nim-eldoc--get-pragma thing)))
    (when (and desc (string< "" desc))
      (format "%s: %s" thing (nim-eldoc--get-pragma thing)))))

(defun nim-eldoc-inside-paren-p ()
  (save-excursion
    (let ((ppss (syntax-ppss)))
      (and (< 0 (nth 0 ppss))
           (eq ?\( (char-after (nth 1 ppss)))))))

;; backward compatibility
(defalias 'nim-eldoc-setup 'ignore)

(provide 'nim-eldoc)
;;; nim-eldoc.el ends here
