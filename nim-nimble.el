;;; nim-nimble.el --- Providing nimble commands -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Maxime Bloch
;;
;; Author: Maxime Bloch <http://github/maxime>
;; Maintainer: Maxime Bloch <me@mcbloch.dev>
;; Created: August 20, 2020
;; Modified: August 20, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/maxime/nim-nimble
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(defun nimble-run ()
  "Run the nimble project"
  (interactive)
  (when (derived-mode-p 'nim-mode)
    (start-process "nimble" "*nimble output*" "nimble" "run")))


(provide 'nim-nimble)
;;; nim-nimble.el ends here
