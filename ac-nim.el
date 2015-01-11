;;; ac-nim.el --- auto-complete source for nim

;; Copyright (C) 2015  Simon Hafner

;; Author: Simon Hafner
;; Maintainer: Simon Hafner <hafnersimon@gmail.com>
;; Package-Version: 0.1
;; Package-Requires: ((auto-complete "1.4") (nim-mode "0.1.5"))
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

(require 'auto-complete)
(require 'nim-mode)

(defcustom ac-nim-type-abbrevs '(
                                 ("skProc" . "f")
                                 ("skIterator" . "i")
                                 ("skTemplate" . "T")
                                 ("skType" . "t")
                                 ("skMethod" . "f")
                                 ("skEnumField" . "e")
                                 ("skGenericParam" . "p")
                                 ("skParam" . "p")
                                 ("skModule" . "m")
                                 ("skConverter" . "C")
                                 ("skMacro" . "M")
                                 ("skField" . "F")
                                 ("skForVar" . "v")
                                 ("skVar" . "v")
                                 ("skLet" . "v")
                                 ("skLabel" . "l")
                                 ("skConst" . "c")
                                 ("skResult" . "r")
                                 )
  "Abbrevs for auto-complete."
  :type 'assoc
  :group 'nim)

;;;###autoload
(eval-after-load 'auto-complete
  '(add-to-list 'ac-modes 'nim-mode))

;;;###autoload
(defun ac-nim-enable ()
  "Add the nim completion source to `ac-sources'."
  (add-hook 'ac-sources 'ac-source-nim-completions nil t))

;;; Some copy/paste from ensime.
(ac-define-source nim-completions
  '((available . ac-nim-available-p)
    (candidates . (ac-nim-completion-candidates ac-prefix))
    (prefix . ac-nim-completion-prefix)
    (action . (lambda ()))                   ; TODO
    (requires . 0)
    ))

(defun ac-nim-available-p ()
  "Return non-nil if this completion source can be enabled."
  (executable-find nim-command))

(defun ac-nim-completion-prefix ()
  "Starting at current point, find the point of completion."
  (let ((point (re-search-backward "\\(\\W\\|[\t ]\\)\\([^\\. ]*\\)?"
                   (point-at-bol) t)))
    (if point (1+ point))))

(defun ac-nim-completion-candidates (prefix)
  (let ((suggestions (nim-call-and-parse-idetools 'suggest)))
    (mapcar (lambda (entry)
              (propertize (nim-ide-name entry)
                          'value entry
                          'symbol (assoc-default (nim-ide-type entry)
                                                 ac-nim-type-abbrevs)
                          'type-sig (nim-ide-signature entry)
                          'summary (ac-nim-trunc-summary (nim-ide-comment entry))
                          ))
            suggestions)))

;;; Copy/pasted from ensime
(defun ac-nim-trunc-summary (str)
  (let ((len (length str)))
    (if (> len 40)
        (concat (substring str 0 40) "...")
      str)))


(provide 'ac-nim)
;;; ac-nim.el ends here
