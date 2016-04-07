;;; nim-compile.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:
(require 'cl-lib)
(require 'rx)
(require 'nim-vars)

;; MEMO:
;; Implemented based on compiler document:
;;   http://nim-lang.org/docs/nimc.html#compiler-usage-configuration-files

;; (5) for PROJECT/PROJECT.(nim.cfg/.nims)
(defun nim-get-project-file (candidates &optional dir)
  (let* ((projdir (file-name-base
                   (directory-file-name (or dir default-directory))))
         (projfile
          (directory-file-name (mapconcat 'file-name-as-directory
                                          `(,default-directory ,projdir)
                                          ""))))
    (cl-loop for ext in candidates
             for file = (format "%s%s" projfile ext)
             if (file-exists-p file)
             do (cl-return file))))

;; (3,4) (parentDir or projectDir)/nim.cfg
(defconst nim-config-regex
  (rx (group (or (group (or "nimcfg" "nim.cfg"))
                 (group (? (and (0+ any) ".")) "nim.cfg"))
             line-end)))

(defun nim-find-config-file ()
  "Get the config file from current directory hierarchy.
The config file would one of those: config.nims, PROJECT.nim.cfg, or nim.cfg."
  (nim-find-file-in-heirarchy
   (file-name-directory (buffer-file-name))
   nim-config-regex))

(defun nim-find-file-in-heirarchy (current-dir pattern)
  "Search for a file matching PATTERN upwards through the directory
hierarchy, starting from CURRENT-DIR"
  (catch 'found
    (locate-dominating-file
     current-dir
     (lambda (dir)
       (let ((pfile (nim-get-project-file '(".nims" ".nim.cfg") dir)))
         (when pfile (throw 'found pfile)))
       (let ((file (cl-first (directory-files dir t pattern nil))))
         (when file (throw 'found file)))))))

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
