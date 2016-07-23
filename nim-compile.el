;;; nim-compile.el --- A support package of compilation for Nim -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:
(require 'cl-lib)
(require 'rx)
(require 'nim-vars)

(defvar nim-compile-command-checker-functions
  '(nim-compile--project)
  "Checker functions to decide build command.
Functions (hooks) take one argument as file file string and
return build command liek ‘nim c -r FILE.nim’")

(defvar nim-compile-default-command
  '("c" "-r" "--verbosity:0" "--hint[Processing]:off"))

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

;; Compile command support
(require 'compile)

(defun nim-compile--project (file)
  "Return ‘nim build FILE’ if there is PROJECT.nims."
  (let ((proj (nim-get-project-file '(".nims" ".nim.cfg"))))
    (when (and proj (nim-nims-file-p proj)
               (eq major-mode 'nim-mode))
      (nim--fmt '("build") file))))

(defun nim-nims-file-p (file)
  (equal "nims" (file-name-extension file)))

(defun nim-nimble-file-p (file)
  (equal "nimble" (file-name-extension file)))

(defun nim-compile--set-compile-command ()
  "Set ‘compile-command’ for Nim language."
  (let ((file (when buffer-file-name
                (shell-quote-argument buffer-file-name)))
        cmd)
    (when file
      (setq cmd
            (cond
             ((eq 'nimscript-mode major-mode)
              (let ((pfile (nim-get-project-file '(".nims" ".nimble"))))
                (cond
                 ;; as build tool
                 ((nim-nimble-file-p file)
                  (let ((nim-compile-command "nimble"))
                    (nim--fmt '("build") "")))
                 ((and (nim-nims-file-p pfile)
                       (equal pfile buffer-file-name))
                  (nim--fmt '("build") pfile))
                 (t
                  ;; as script file
                  (nim--fmt '("e") file)))))
             (t
              (let ((cmd (run-hook-with-args-until-success
                          'nim-compile-command-checker-functions file)))
                (or cmd (nim--fmt nim-compile-default-command file))))))
      (setq-local compile-command
                  (if (or compilation-read-command current-prefix-arg)
                      (compilation-read-command cmd)
                    cmd)))))

(defun nim--fmt (args file)
  (mapconcat
   'shell-quote-argument
   (delq nil `(,nim-compile-command ,@args ,@nim-compile-user-args ,file))
   " "))

(define-derived-mode nim-compile-mode compilation-mode "nim-compile"
  "major-mode for nim compilation buffer.")

;;;###autoload
(defun nim-compile ()
  (interactive)
  (when (derived-mode-p 'nim-mode)
    (add-hook 'compilation-filter-hook 'nim--colorize-compilation-buffer)
    (add-hook 'compilation-finish-functions 'nim--remove-colorize-hook)
    (nim-compile--set-compile-command)
    (funcall 'compile compile-command 'nim-compile-mode)))

(require 'ansi-color)
(defun nim--colorize-compilation-buffer ()
  "Colorize compilation buffer."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point-max))))

(defun nim--remove-colorize-hook (_buf _process-state)
  "Remove ‘nim--colorize-compilation-buffer’."
  (when (get-buffer "*nim-compile*")
    (remove-hook 'compilation-filter-hook 'nim--colorize-compilation-buffer)
    (remove-hook 'compilation-finish-functions 'nim--remove-colorize-hook)))

(provide 'nim-compile)
;;; nim-compile.el ends here
