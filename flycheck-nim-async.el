;;; flycheck-nim-async.el --- Yet another flycheck plugin for Nim language -*- lexical-binding: t; -*-

;; Copyright (C) 2016 by Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>

;;; License:
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; To use this plugin correctly, you need load this file before
;; loading ‘flycheck’ because this flycheck-nim-async plugin needs to
;; override ‘flycheck-hooks-alist’ variable.

;; This plugin needs this nimsuggest:
;;   https://github.com/nim-lang/nimsuggest/pull/22

;; Also you might need this configuration
;;
;;   (setq nimsuggest-vervosity nil)

;;; Code:

(defadvice flycheck-mode (around nimsuggest-override-hooks activate)
  "Override ‘flycheck-hooks-alist’."
  (when (derived-mode-p 'nim-mode)
    (setq-local
     flycheck-hooks-alist
     (cl-loop with requires = '(flycheck-error-list-update-source
                                flycheck-error-list-highlight-errors
                                flycheck-display-error-at-point-soon
                                flycheck-hide-error-buffer
                                flycheck-display-error-at-point)
              for (hook . func) in flycheck-hooks-alist
              if (member func requires)
              collect (cons hook func)
              else if (member hook '(after-save-hook after-change-functions))
              collect (cons hook 'flycheck-nim-async-after-change)
              else if (member hook '(post-command-hook))
              collect (cons hook 'flycheck-epc-async-post-command))))
  ad-do-it)

(require 'flycheck)
(require 'nim-suggest)
(require 'cl-lib)

(defun flycheck-epc-define-checker (checker parser patterns base-func)
  "Define flycheck CHECKER for EPC based on PARSER and PATTERNS.

The BASE-FUNC is a function that returns string."
  (let ((command   '("echo dummy command")))
    (pcase-dolist
        (`(,prop . ,value)
         `((flycheck-command . ,command)
           (flycheck-error-parser . ,parser)
           (flycheck-error-patterns
            . ,(mapcar (lambda (p)
                         (cons (flycheck-rx-to-string `(and ,@(cdr p)) 'no-group)
                               (car p)))
                       patterns))))
      (put checker prop value)))
  (put 'nimsuggest-async 'flycheck-epc-base-func base-func)
  (add-to-list 'flycheck-checkers checker))

(defvar-local flycheck-epc-timer nil)
(defvar flycheck-epc-timer-interval 2)
(defun flycheck-epc-async-post-command (&rest _rst)
  "Call async checker after command."
  (let ((f (get 'nimsuggest-async 'flycheck-epc-base-func)))
    (when f
      (when (and flycheck-epc-timer
                 (timerp flycheck-epc-timer))
        (cancel-timer flycheck-epc-timer))
      (setq flycheck-epc-timer
            (run-with-timer flycheck-epc-timer-interval nil f)))))

(defun flycheck-epc-highlight ()
  "Highlight (overlay) errors based on ‘flycheck-current-errors’."
  (mapc (lambda (err) (flycheck-add-overlay err))
        flycheck-current-errors))

;; For nim specific

(flycheck-epc-define-checker
 'nimsuggest-async
 'flycheck-parse-with-patterns
 '((error line-start (file-name) "(" line ", " column ") "
                 "Error:"
                 (message (one-or-more not-newline)
                          (optional
                           (and "\nbut expected one of:"
                                (minimal-match (one-or-more anything))
                                "\n\n"))))
          (warning line-start (file-name) "(" line ", " column ") "
                   (or "Hint:" "Warning:") (message) line-end))
 'flycheck-nim-async)

(defun flycheck-nim-async (&optional force)
  "Check current buffer using nimsuggest ’chk option."
  (interactive)
  (when (and nim-nimsuggest-path (derived-mode-p 'nim-mode))
    (save-excursion
      (goto-char (point-max))
      (nim-call-epc
       'chk
       (if force
           'flycheck-nim-async-force-update
         'flycheck-nim-async-callback)))))

(defun flycheck-nim-async-after-change (&rest _rst)
  "Clear highlight and update."
  (flycheck-nim-async t))

(defvar nimsuggest-check-output "")
(defun flycheck-nim-async-callback (output &optional force)
  (setq nimsuggest-check-output (format "%s" output))
  (flycheck-nim-async-update nimsuggest-check-output force))

(defun flycheck-nim-async-update (output &optional force)
  (when (derived-mode-p 'nim-mode)
    (when (or (flycheck-nim-async-update-maybe output force))
      (let ((saved flycheck-current-errors))
        (flycheck-clear)
        (setq flycheck-current-errors saved))
      (flycheck-epc-highlight))))

(defun flycheck-nim-async-force-update (&optional output)
  (when output (flycheck-nim-async-callback output t)))

(defun flycheck-nim-async-update-maybe (&optional output force)
  (when output
    (let ((old-errors flycheck-current-errors)
          (new-errors (flycheck-nim-async-filter output)))
      (when (or (not (equal old-errors new-errors)) force)
        (setq flycheck-current-errors new-errors)
        flycheck-current-errors))))

(defun flycheck-nim-async-filter (&optional output)
  (flycheck-parse-with-patterns
   (format "%s" (or output nimsuggest-check-output))
   'nimsuggest-async (current-buffer)))

(provide 'flycheck-nim-async)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; flycheck-nim-async.el ends here
