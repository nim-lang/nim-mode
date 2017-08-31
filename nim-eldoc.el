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

;; Eldoc supports for Nim.  This package automatically turns on
;; if you set ‘nim-nimsuggest-path’ and the nimsuggest is working.

;;; Code:

(require 'nim-vars)
(require 'nim-suggest)
(require 'nim-helper)
(require 'cl-lib)
(require 'eldoc)

(defvar nim-eldoc--data nil)
(defvar nim-eldoc--skip-regex
  (rx (or (group symbol-start
                 (or "if" "when" "elif" "while"
                     ;; for tuple assignment
                     "var" "let" "const")
                 symbol-end (0+ " "))
          (group line-start (0+ " ")))))

;;;###autoload
(defun nim-eldoc-function ()
  "Return a doc string appropriate for the current context, or nil."
  (interactive)
  (when (and (or (bound-and-true-p eldoc-mode)
                 ;; This mode was added at Emacs 25
                 (bound-and-true-p global-eldoc-mode))
             (not (eq ?\s (char-after (point)))))
    (if (nim-inside-pragma-p)
        (nim-eldoc--pragma-at-point)
      (unless (nim-eldoc-same-try-p)
        (nim-eldoc--call-nimsuggest))
      (when (eq (line-number-at-pos)
                (assoc-default :line nim-eldoc--data))
        (assoc-default :str nim-eldoc--data)))))

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

(defun nim-eldoc--call-nimsuggest ()
  (save-excursion
    (nim-eldoc--move)
    (nim-call-epc
     ;; version 2 protocol can use: ideDef, ideUse, ideDus
     'dus 'nim-eldoc--update)))

(defun nim-eldoc--move ()
  (let ((pos  (point))
        (ppss (syntax-ppss)))
    (when (nim-eldoc-inside-paren-p)
      (goto-char (nth 1 ppss))
      (when (looking-back nim-eldoc--skip-regex nil)
        (goto-char pos)))))

(defun nim-eldoc-inside-paren-p ()
  (save-excursion
    (let ((ppss (syntax-ppss)))
      (and (< 0 (nth 0 ppss))
           (eq ?\( (char-after (nth 1 ppss)))))))

(defun nim-eldoc-same-try-p ()
  (or (and (equal (nim-current-symbol)
                  (assoc-default :name nim-eldoc--data))
           (eq (assoc-default :line nim-eldoc--data)
               (line-number-at-pos)))
      (and (nim-eldoc-inside-paren-p)
           (save-excursion
             (nim-eldoc--move)
             (or
              ;; for template
              (eq (point) (assoc-default :pos nim-eldoc--data))
              ;; for proc
              (eq (1- (point)) (assoc-default :pos nim-eldoc--data)))))))

(defun nim-eldoc--update (defs)
  (if defs
      (nim-eldoc--update-1 defs)
    (save-excursion
      (when (nim-eldoc-inside-paren-p)
        (nim-eldoc--move)
        (backward-char)
        (nim-call-epc 'dus 'nim-eldoc--update-1)))))

(defun nim-eldoc--update-1 (defs)
  (when defs
    (setq nim-eldoc--data
          (list
           (cons :str  (nim-eldoc-format-string defs))
           (cons :line (line-number-at-pos))
           (cons :name (nim-current-symbol))
           (cons :pos  (point))))
    (setq eldoc-last-message (assoc-default :str nim-eldoc--data))
    (message eldoc-last-message)))

(defun nim-eldoc-format-string (defs)
  "Format data inside DEFS for eldoc.
DEFS is group of definitions from nimsuggest."
  ;; TODO: switch if there are multiple defs
  (let* ((data    (cl-first defs))
         (forth   (nim-epc-forth         data))
         (symKind (nim-epc-symkind       data))
         (qpath   (nim-epc-qualifiedPath data))
         (doc     (nim-epc-doc           data)))
    (nimsuggest-format forth symKind qpath doc)))

;; backward compatibility
(defalias 'nim-eldoc-setup 'ignore)

(provide 'nim-eldoc)
;;; nim-eldoc.el ends here
