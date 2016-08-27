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

(defvar nimscirpt-mode-syntax-table
  (copy-syntax-table nim-mode-syntax-table))

;;;###autoload
(define-derived-mode nimscript-mode prog-mode "NimScript"
  "A major-mode for NimScript files.
This major-mode is activated when you enter *.nims and *.nimble
suffixed files, but if it’s .nimble file, also another logic is
applied. See also ‘nimscript-mode-maybe’."
  :group 'nim

  (nim--common-init)

  (nim--set-font-lock-keywords 'nimscript-mode))

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
