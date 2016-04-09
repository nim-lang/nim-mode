;;; nim-suggest.el --- a plugin to use nimsuggest from Emacs -*- lexical-binding: t -*-
;;; Commentary:

;;

;;; Code:

(require 'nim-vars)
(require 'epc)
(require 'cl-lib)
(require 'nim-compile)

;;; If you change the order here, make sure to change it over in
;;; nimsuggest.nim too.
(defconst nim-epc-order
  '(:section :symkind :qualifiedPath :filePath :forth :line :column :doc))

(cl-defstruct nim-epc
  section symkind qualifiedPath filePath forth line column doc)

(defun nim-parse-epc (obj method)
  "Parse OBJ according to METHOD."
  (cl-case method
    (chk obj)
    ((sug con def use dus)
     (cl-mapcar
      (lambda (sublist)
        (apply #'make-nim-epc (cl-mapcan #'list nim-epc-order sublist)))
      obj))))

(defvar nim-epc-processes-alist nil)

(defvar nimsuggest-get-option-function nil
  "Function to get options for nimsuggest.")

(defun nimsuggest-get-options (main-file)
  (delq nil
        (append nim-suggest-options nim-suggest-local-options
                (when (eq 'nimscript-mode major-mode)
                  '("--define:nimscript" "--define:nimconfig"))
                (list (with-no-warnings nimsuggest-vervosity)
                      "--epc" main-file))))

(defun nim-find-project-main-file ()
  (or (and (eq 'nimscript-mode major-mode)
           buffer-file-name)
      (nim-find-config-file)
      buffer-file-name))

(defun nim-find-or-create-epc ()
  "Get the epc responsible for the current buffer."
  (let ((main-file (nim-find-project-main-file)))
    (or (let ((epc-process (cdr (assoc main-file nim-epc-processes-alist))))
          (if (eq 'run (epc:manager-status-server-process epc-process))
              epc-process
            (prog1 ()
              (nim-suggest-kill-zombie-processes main-file))))
        (let ((epc-process
               (epc:start-epc
                nim-nimsuggest-path
                (nimsuggest-get-options main-file))))
          (push (cons main-file epc-process) nim-epc-processes-alist)
          epc-process))))

(defun nim-call-epc (method callback)
  "Call the nimsuggest process on point.

Call the nimsuggest process responsible for the current buffer.
All commands work with the current cursor position.  METHOD can be
one of:

sug: suggest a symbol
con: suggest, but called at fun(_ <-
def: where the symbol is defined
use: where the symbol is used
dus: def + use

The CALLBACK is called with a list of ‘nim-epc’ structs."
  (unless nim-inside-compiler-dir-p
    (let ((tempfile (nim-save-buffer-temporarly)))
      (deferred:$
        (epc:call-deferred
         (nim-find-or-create-epc)
         method
         (cl-case method
           (chk
            (list (buffer-file-name)
                  -1 -1
                  tempfile))
           (t
            (list (buffer-file-name)
                  (line-number-at-pos)
                  (current-column)
                  tempfile))))
        (deferred:nextc it
          (lambda (x) (funcall callback (nim-parse-epc x method))))
        (deferred:watch it
          (lambda (_) (delete-directory (file-name-directory tempfile) t)))))))

(defvar nim-dirty-directory
  ;; Even users changed the temp directory name,
  ;; ‘file-name-as-directory’ ensures suffix directory separator.
  (mapconcat 'file-name-as-directory
             `(,temporary-file-directory "emacs-nim-mode") "")
  "Directory name, which nimsuggest uses temporarily.
Note that this directory is removed when you exit from Emacs.")


(defun nim-suggest-get-temp-file-name ()
  (mapconcat 'directory-file-name
             `(,nim-dirty-directory ,buffer-file-name)
             ""))

(defun nim-make-tempdir (tempfile)
  (let* ((tempdir (file-name-directory tempfile)))
    (unless (file-exists-p tempdir)
      (make-directory tempdir t))))

(defun nim-save-buffer-temporarly ()
  "Save the current buffer and return the location, so we
can pass it to epc."
  (let* ((temporary-file-directory nim-dirty-directory)
         (filename (nim-suggest-get-temp-file-name)))
    (nim-make-tempdir filename)
    (save-restriction
      (widen)
      (write-region (point-min) (point-max) filename nil 1))
    filename))

(add-hook 'kill-emacs-hook 'nim-delete-nimsuggest-temp-directory)
(defun nim-delete-nimsuggest-temp-directory ()
  "Delete temporary files directory for nimsuggest."
  (when (file-exists-p nim-dirty-directory)
    (delete-directory (file-name-directory nim-dirty-directory) t)))

(defun nim-suggest-kill-zombie-processes (&optional mfile)
  (setq nim-epc-processes-alist
        (cl-loop for (file . manager) in nim-epc-processes-alist
                 if (and (epc:live-p manager)
                         (or (and mfile (equal mfile file))
                             (not mfile)))
                 collect (cons file manager)
                 else do (epc:stop-epc manager))))

(defun nim-goto-sym ()
  "Go to the definition of the symbol currently under the cursor."
  (interactive)
  (nim-call-epc 'def
                (lambda (defs)
                  (let ((def (cl-first defs)))
                    (when (not def) (error "Symbol not found"))
                    (find-file (nim-epc-filePath def))
                    (goto-char (point-min))
                    (forward-line (1- (nim-epc-line def)))))))

(provide 'nim-suggest)
;;; nim-suggest.el ends here
