;;; nimscript-mode.el --- A major mode for the NimScript -*- lexical-binding: t -*-

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

;;; Code:
(require 'nim-vars)
(require 'nim-syntax)
(require 'nim-mode)

(defconst nimscript-keywords
  (append
   `(,(cons (nim--format-keywords 'nimscript-builtins)
            font-lock-builtin-face)
     ,(cons (nim--format-keywords 'nimscript-variables)
            font-lock-variable-name-face))
   `((,(rx symbol-start "task" symbol-end (1+ " ")
           (group  symbol-start (or "build" "tests" "bench") symbol-end))
      (1 font-lock-builtin-face))
     ("\\_<ScriptMode\\_>" (0 font-lock-type-face)))))

;;;###autoload
(define-derived-mode nimscript-mode nim-mode "NimScript"
  "A major-mode for NimScript files.
This major-mode is activated when you enter *.nims and *.nimble
suffixed files, but if it’s .nimble file, also another logic is
applied. See also ‘nimscript-mode-maybe’."
  :group 'nim
  (setq-local font-lock-defaults
              `(,(append
                  nim-font-lock-keywords
                  nim-font-lock-keywords-extra
                  nim-font-lock-keywords-2
                  ;; Add extra keywords for NimScript
                  nimscript-keywords)
                nil nil nil nil
                (font-lock-syntactic-face-function
                 . nim-font-lock-syntactic-face-function))))

;;;###autoload
(defun nimscript-mode-maybe ()
  "Most likely turn on ‘nimscript-mode’.
In *.nimble files, if the first line sentence matches
‘nim-nimble-ini-format-regex’, this function activates ‘conf-mode’
instead.  The default regex’s matching word is [Package]."
  (interactive)
  (if (not (buffer-file-name))
      (nimscript-mode)
    (let ((extension (file-name-extension (buffer-file-name))))
      (cond ((equal "nims" extension)
             (nimscript-mode))
            ((equal "nimble" extension)
             (save-excursion
               (goto-char (point-min))
               (if (looking-at-p nim-nimble-ini-format-regex)
                   (conf-mode)
                 (nimscript-mode))))))))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.nim\\(ble\\|s\\)\\'" . nimscript-mode-maybe))

(provide 'nimscript-mode)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; nimscript-mode.el ends here
