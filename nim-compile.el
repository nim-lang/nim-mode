;;; nim-compile.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(defcustom nim-project-root-regex "\\(\.git\\|\.nim\.cfg\\|\.nimble\\)$"
  "Regex to find project root directory."
  :type 'string
  :group 'nim)

(defun nim-get-project-root ()
  "Return project directory."
  (file-name-directory
   (nim-find-file-in-heirarchy
    (file-name-directory (buffer-file-name)) nim-project-root-regex)))

(provide 'nim-compile)
;;; nim-compile.el ends here
