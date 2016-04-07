;;; nim-compile.el --- A support package of compilation for Nim -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:
(require 'cl-lib)
(require 'nim-vars)

(defvar nim-compile-command-checker-functions
  '(nim-compile--project nim-compile--makefile))

(defvar nim-compile-buffer-name "*Nim Compile*")

;; MEMO:
;; Implement based on compiler document:
;;   http://nim-lang.org/docs/nimc.html#compiler-usage-configuration-files

;; (5) for PROJECT/PROJECT.(nim.cfg/.nims)
(defun nim-get-project-file ()
  (let* ((projdir (file-name-base (directory-file-name default-directory)))
         (projfile
          (directory-file-name (mapconcat 'file-name-as-directory
                                          `(,default-directory ,projdir)
                                          ""))))
    (cl-loop for ext in '(".nims" ".nim.cfg")
             for file = (format "%s%s" projfile ext)
             if (file-exists-p file)
             do (cl-return file))))

;; (3,4) (parentDir or projectDir)/nim.cfg
(defconst nim-config-regex
  (rx (group (or
              (group (or "nimcfg" "nim.cfg"))
              (group (? (and (0+ any) ".")) "nim.cfg"))
             line-end)))

;; (2) User config
(defvar nim-user-conf-file nil)
(defun nim-get-conffile-from-user-dir ()
  (let ((dir (or (file-name-as-directory (getenv "XDG_CONFIG_HOME"))
                 (mapconcat 'file-name-as-directory
                            `(,(getenv "HOME") ".config") ""))))
    (delq nil
          (mapcar (lambda (file)
                    (let ((cfgfile (format "%s%s" dir file)))
                      (when (file-exists-p cfgfile)
                        cfgfile)))
                  '("nimcfg" "nim.cfg")))))
(setq-default nim-user-conf-file (nim-get-conffile-from-user-dir))

;; (1) $nim/config/nim.cfg, /etc/nim.cfg
(defvar nim-compier-config-file nil)
(setq-default
 nim-compier-config-file
 (let ((nim-conf
        (format
         "%s%snim.cfg"
         (file-name-directory
          (directory-file-name (file-name-directory (executable-find "nim"))))
         (file-name-as-directory "config"))))
   (when (file-exists-p nim-conf)
     nim-conf)))

(defun nim-find-config-file ()
  "Get the config file from current directory hierarchy.
The config file would one of those: config.nims, PROJECT.nim.cfg, or nim.cfg."
  (or (nim-get-project-file)
      (nim-find-file-in-heirarchy
       (file-name-directory (buffer-file-name))
       nim-config-regex)
      nim-user-conf-file
      nim-compier-config-file))

(defun nim-find-file-in-heirarchy (current-dir pattern)
  "Search for a file matching PATTERN upwards through the directory
hierarchy, starting from CURRENT-DIR"
  (catch 'found
    (locate-dominating-file
     current-dir
     (lambda (dir)
       (let ((file (car (directory-files dir t pattern nil))))
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


(require 'compile)
(defun nim-compile--project (file)
  "Return ‘nim build FILE’ if there is PROJECT.nims."
  (let ((proj (nim-get-project-file)))
    (when (and proj (equal "nims" (file-name-extension proj))
               (eq major-mode 'nim-mode))
      (nim--cmd '("build") file))))

(defun nim-compile--makefile (file)
  "Return ‘make -k FILE’ if there is [mM]akefile."
  (when (or (file-exists-p "makefile")
            (file-exists-p "Makefile"))
    (format "make -k %s" file)))

(defun nim-compile--set-compile-command ()
  "Set ‘compile-command’ for Nim language."
  (let ((file (when buffer-file-name
                (shell-quote-argument
                 (file-name-sans-extension buffer-file-name)))))
    (when file
      (setq-local compile-command
                  (cond
                   ((eq 'nimscript-mode major-mode)
                    (nim--cmd '("e") file))
                   (t
                    (let ((cmd (run-hook-with-args-until-success
                                'nim-compile-command-checker-functions file)))
                      (or cmd (nim--cmd '("c" "-r") file)))))))))

(defun nim--cmd (args file)
  "Execute ARGS with FILE by Nim compiler."
  (mapconcat
   'shell-quote-argument
   (delq nil `(,nim-compile-command ,@args ,@nim-compile-user-args ,file))
   " "))

;;;###autoload
(defun nim-compile ()
  (interactive)
  (when (derived-mode-p 'nim-mode)
    (add-hook 'compilation-filter-hook 'nim--colorize-compilation-buffer)
    (add-hook 'compilation-finish-functions 'nim--remove-colorize-hook)
    (nim-compile--set-compile-command)
    (compilation-start
     compile-command nil (lambda (_) nim-compile-buffer-name))))

(require 'ansi-color)
(defun nim--colorize-compilation-buffer ()
  "Colorize compilation buffer."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point-max))))

(defun nim--remove-colorize-hook (_buf _process-state)
  "Remove ‘nim--colorize-compilation-buffer’."
  (when (get-buffer nim-compile-buffer-name)
    (remove-hook 'compilation-filter-hook 'nim--colorize-compilation-buffer)
    (remove-hook 'compilation-finish-functions 'nim--remove-colorize-hook)))

(provide 'nim-compile)
;;; nim-compile.el ends here
