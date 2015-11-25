;;; nim-suggest.el --- -*- lexical-binding: t -*-
;;; Commentary:

;;

;;; Code:

;;; Completion


(require 'epc)
(require 'cl-lib)

(defcustom nim-nimsuggest-path nil "Path to the nimsuggest binary."
  :type 'string
  :group 'nim)

(defcustom nim-project-root-regex "\\(\.git\\|\.nim\.cfg\\|\.nimble\\)$"
  "Regex to find project root directory."
  :type 'string
  :group 'nim)

(defun nim-find-file-in-heirarchy (current-dir pattern)
  "Search for a file matching PATTERN upwards through the directory
hierarchy, starting from CURRENT-DIR"
  (catch 'found
    (locate-dominating-file
     current-dir
     (lambda (dir)
       (let ((file (cl-first (directory-files dir t pattern nil))))
         (when file (throw 'found file)))))))

(defun nim-find-project-main-file ()
  "Get the main file for the project."
  (let ((main-file (nim-find-file-in-heirarchy
                (file-name-directory (buffer-file-name))
                ".*\.nim\.cfg")))
    (when main-file (file-name-base main-file))))

(defun nim-get-project-root ()
  "Return project directory."
  (file-name-directory
   (nim-find-file-in-heirarchy (file-name-directory (buffer-file-name)) nim-project-root-regex)))

;;; If you change the order here, make sure to change it over in
;;; nimsuggest.nim too.
(defconst nim-epc-order '(:section :symkind :qualifiedPath :filePath :forth :line :column :doc))

(cl-defstruct nim-epc section symkind qualifiedPath filePath forth line column doc)
(defun nim-parse-epc (list)
  ;; (message "%S" list)
  (cl-mapcar (lambda (sublist) (apply #'make-nim-epc
                               (cl-mapcan #'list nim-epc-order sublist)))
          list))

(defvar nim-epc-processes-alist nil)

(defun nim-find-or-create-epc ()
  "Get the epc responsible for the current buffer."
  (let ((main-file (or (nim-find-project-main-file)
                           (buffer-file-name))))
    (or (let ((epc-process (cdr (assoc main-file nim-epc-processes-alist))))
          (if (eq 'run (epc:manager-status-server-process epc-process))
              epc-process
            (progn (setq nim-epc-processes-alist (assq-delete-all main-file nim-epc-processes-alist))
                   nil)))
        (let ((epc-process (epc:start-epc nim-nimsuggest-path (list "--verbosity:0" "--epc" main-file))))
          (push (cons main-file epc-process) nim-epc-processes-alist)
          epc-process))))

(defun nim-call-epc (method callback)
  "Call the nimsuggest process on point.

Call the nimsuggest process responsible for the current buffer.
All commands work with the current cursor position.  METHOD can be
one of:

sug: suggest a symbol
con: suggest, but called at fun(_ <-
def: where the is defined
use: where the symbol is used

The callback is called with a list of nim-epc structs."
  (let ((tempfile (nim-save-buffer-temporarly)))
    (deferred:$
      (epc:call-deferred
       (nim-find-or-create-epc)
       method
       (list (buffer-file-name)
             (line-number-at-pos)
             (current-column)
             tempfile))
      (deferred:nextc it
        (lambda (x) (funcall callback (nim-parse-epc x))))
      (deferred:watch it (lambda (_x) (delete-directory (file-name-directory tempfile) t))))))

(defun nim-save-buffer-temporarly ()
  "Save the current buffer and return the location, so we
can pass it to epc."
  (let* ((dirname (make-temp-file "nim-dirty" t))
         (filename (expand-file-name (file-name-nondirectory (buffer-file-name))
                                     (file-name-as-directory dirname))))
    (save-restriction
      (widen)
      (write-region (point-min) (point-max) filename nil 1))
    filename))

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
