;;; flycheck-nimsuggest.el --- Yet another flycheck plugin for Nim language -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017 by Yuta Yamada

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

;; On the fly syntax check support using Nimsuggest and flycheck.el.

;; This plugin needs this nimsuggest:
;;   https://github.com/nim-lang/nimsuggest/pull/22

;; TODO: add README how to use
;; so far I implemented this package automatically turned on if user
;; satisfied ‘nim-suggest-available-p’, but it may need users’ feedback.

;; memo:
;; https://github.com/lunaryorn/blog/blob/master/posts/generic-syntax-checkers-in-flycheck.md

;;; Code:

(require 'flycheck)
(require 'cl-lib)
(require 'nim-suggest)

(defvar flycheck-nimsuggest-patterns
  (mapcar (lambda (p)
            (cons (flycheck-rx-to-string `(and ,@(cdr p)) 'no-group)
                  (car p)))
         `((error line-start (file-name) "(" line ", " column ") "
           "Error:"
           (message (one-or-more not-newline)
                    (optional
                     (and "\nbut expected one of:"
                          (minimal-match (one-or-more anything))
                          "\n\n"))))
           (warning line-start (file-name) "(" line ", " column ") "
                    (or "Hint:" "Warning:") (message) line-end))))
(put 'nim-nimsuggest 'flycheck-error-patterns flycheck-nimsuggest-patterns)

;;;###autoload
(defun flycheck-nim-nimsuggest-start (checker callback)
  "Start nimsuggest’s ‘chk’ method syntax check with CHECKER.

CALLBACK is the status callback passed by Flycheck."
  ;; The syntax checker being run, just in case the :start function is
  ;; being shared among different syntax checkers.
  ;; A callback function, which shall be used to report the results of a
  ;; syntax check back to Flycheck.
  (let ((buffer (current-buffer)))
    (nim-call-epc
     'chk
     (lambda (errors)
       (condition-case err
           (let ((res (flycheck-parse-with-patterns errors checker buffer)))
             (funcall callback 'finished (delq nil res)))
         ;; TODO: add proper error handler
         (error (funcall callback 'errored err)))))))

;;;###autoload
(eval-after-load "flycheck"
  '(progn
     (flycheck-define-generic-checker 'nim-nimsuggest
       "A syntax checker for Nim lang using nimsuggest.

See URL `'."
       :start 'flycheck-nim-nimsuggest-start
       :modes '(nim-mode nimscript-mode)
       :predicate (lambda () (and (nim-suggest-available-p)
                             nim-use-flycheck-nimsuggest)))

     (add-to-list 'flycheck-checkers 'nim-nimsuggest)))

(provide 'flycheck-nimsuggest)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; flycheck-nimsuggest.el ends here
