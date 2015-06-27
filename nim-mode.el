;;; nim-mode.el --- A major mode for the Nim programming language
;;
;; Filename: nim-mode.el
;; Description: A major mode for the Nim programming language
;; Author: Simon Hafner
;; Maintainer: Simon Hafner <hafnersimon@gmail.com>
;; Version: 0.2.0
;; Keywords: nim languages
;; Compatibility: GNU Emacs 24
;; Package-Requires: ((emacs "24") (epc "0.1.1"))
;;
;; Taken over from James H. Fisher <jameshfisher@gmail.com>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Large parts of this code is shamelessly stolen from python.el and
;; adapted to Nim
;;
;; Todo:
;;
;; -- Make things non-case-sensitive and ignore underscores
;; -- Identifier following "proc" gets font-lock-function-name-face
;; -- Treat parameter lists separately
;; -- Treat pragmas inside "{." and ".}" separately
;; -- Make double-# comments get font-lock-doc-face
;; -- Highlight tabs as syntax error
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile
  (require 'cl))

(require 'nim-vars)
(require 'nim-syntax)
(require 'nim-util)
(require 'nim-helper)
(require 'nim-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Helpers                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nim-glue-strings (glue strings)
  "Concatenate some GLUE and a list of STRINGS."
  (mapconcat 'identity strings glue))

(defun nim-regexp-choice (strings)
  "Construct a regexp multiple-choice from a list of STRINGS."
  (concat "\\(" (nim-glue-strings "\\|" strings) "\\)"))

(put 'nim-mode 'font-lock-defaults '(nim-font-lock-keywords nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Indentation                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Indentation

(defvar nim-indent-dedenters
  (nim-rx symbol-start
             (or "else" "elif" "of" "finally" "except")
             symbol-end
             (* (not (in "\n")))
             ":" (* space) (or "#" eol))
  "Regular expression matching the end of line after which should be dedented.
If the end of a line matches this regular expression, the line
will be dedented relative to the previous block.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Wrap it all up ...                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(define-derived-mode nim-mode prog-mode "Nim"
  "A major mode for the Nim programming language."
  :group 'nim

  (setq font-lock-defaults '(nim-font-lock-keywords nil t))

  ;; ;; Comment
  ;; (set (make-local-variable 'comment-start) "# ")
  ;; (set (make-local-variable 'comment-start-skip) "#+\\s-*")
  ;; modify the keymap
  (set (make-local-variable 'indent-line-function) 'nim-indent-line-function)
  (set (make-local-variable 'indent-region-function) #'nim-indent-region)
  (setq indent-tabs-mode nil) ;; Always indent with SPACES!
  )

(defcustom nim-compiled-buffer-name "*nim-js*"
  "The name of the scratch buffer used to compile Javascript from Nim."
  :type 'string
  :group 'nim)

(defcustom nim-command "nim"
  "Path to the nim executable.
You don't need to set this if the nim executable is inside your PATH."
  :type 'string
  :group 'nim)

(defcustom nim-args-compile '()
  "The arguments to pass to `nim-command' to compile a file."
  :type '(repeat string)
  :group 'nim)

(defcustom nim-project-root-regex "\\(\.git\\|\.nim\.cfg\\|\.nimble\\)$"
  "Regex to find project root directory."
  :type 'string
  :group 'nim)

(defun nim-get-project-root ()
  "Return project directory."
  (file-name-directory
   (nim-find-file-in-hierarchy (file-name-directory (buffer-file-name)) nim-project-root-regex)))

(defun nim-compile-file-to-js (&optional callback)
  "Save current file and compiles it.
Use the project directory, so it will work best with external
libraries where `nim-compile-region-to-js' does not.  Return the
filename of the compiled file.  The CALLBACK is executed on
success with the filename of the compiled file."
  (interactive)
  (save-buffer)
  (let ((default-directory (or (nim-get-project-root) default-directory)))
    (lexical-let ((callback callback))
      (nim-compile (list "js" (buffer-file-name))
                      (lambda () (when callback
                              (funcall callback (concat default-directory
                                                        "nimcache/"
                                                        (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
                                                        ".js"))))))))

(defun nim-compile-region-to-js (start end)
  "Compile the current region to javascript.
The result is written into the buffer
`nim-compiled-buffer-name'."
  (interactive "r")

  (lexical-let ((buffer (get-buffer-create nim-compiled-buffer-name))
                (tmpdir (file-name-as-directory (make-temp-file "nim-compile" t))))
    (let ((default-directory tmpdir))
      (write-region start end "tmp.nim" nil 'foo)
      (with-current-buffer buffer
        (erase-buffer)
        (let ((default-directory tmpdir))
          (nim-compile '("js" "tmp.nim")
                       (lambda () (with-current-buffer buffer
                               (insert-file-contents
                                (concat tmpdir (file-name-as-directory "nimcache") "tmp.js"))
                               (display-buffer buffer)))))))))

(defun nim-compile (args &optional on-success)
  "Invoke the compiler and call ON-SUCCESS in case of successful compilation."
  (lexical-let ((on-success (or on-success (lambda () (message "Compilation successful.")))))
    (if (bufferp "*nim-compile*")
        (with-current-buffer "*nim-compile*"
          (erase-buffer)))
    ))

(defun nim-doc-buffer (element)
  "Displays documentation buffer with ELEMENT contents."
  (let ((buf (get-buffer-create "*nim-doc*")))
    (with-current-buffer buf
      (view-mode -1)
      (erase-buffer)
      (insert (get-text-property 0 :nim-doc element))
      (goto-char (point-min))
      (view-mode 1)
      buf)))

;;; Completion

(defcustom nim-nimsuggest-path nil "Path to the nimsuggest binary."
  :type 'string
  :group 'nim)

(require 'epc)

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
        (let ((epc-process (epc:start-epc nim-nimsuggest-path (list "--epc" main-file))))
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
  (lexical-let ((tempfile (nim-save-buffer-temporarly))
                (cb callback))
    (deferred:$
      (epc:call-deferred
       (nim-find-or-create-epc)
       method
       (list (buffer-file-name)
             (line-number-at-pos)
             (current-column)
             tempfile))
      (deferred:nextc it
        (lambda (x) (funcall cb (nim-parse-epc x))))
      (deferred:watch it (lambda (x) (delete-directory (file-name-directory tempfile) t))))))

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

(defun nim-find-file-in-hierarchy (current-dir pattern)
  "Search for a file matching PATTERN upwards through the directory
hierarchy, starting from CURRENT-DIR"
  (catch 'found
    (locate-dominating-file
     current-dir
     (lambda (dir)
       (let ((file (first (directory-files dir t pattern nil))))
         (when file (throw 'found file)))))))

(defun nim-find-project-main-file ()
  "Get the main file for the project."
  (let ((main-file (nim-find-file-in-hierarchy
                (file-name-directory (buffer-file-name))
                ".*\.nim\.cfg")))
    (when main-file (file-name-base main-file))))

(defun nim-goto-sym ()
  "Go to the definition of the symbol currently under the cursor."
  (interactive)
  (nim-call-epc 'def
                (lambda (defs)
                  (let ((def (first defs)))
                    (when (not def) (error "Symbol not found"))
                    (find-file (nim-epc-filePath def))
                    (goto-char (point-min))
                    (forward-line (1- (nim-epc-line def)))))))

;; compilation error
(eval-after-load 'compile
  '(progn
     (with-no-warnings
       (add-to-list 'compilation-error-regexp-alist 'nim)
       (add-to-list 'compilation-error-regexp-alist-alist
                    '(nim "^\\s-*\\(.*\\)(\\([0-9]+\\),\\s-*\\([0-9]+\\))\\s-+\\(?:Error\\|\\(Hint\\)\\):" 1 2 3 (4))))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.nim\\'" . nim-mode))

(provide 'nim-mode)

;;; nim-mode.el ends here
