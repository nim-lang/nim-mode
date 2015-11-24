;;; nim-compile.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:
(require 'nim-suggest)

(defcustom nim-command "nim"
  "Path to the nim executable.
You don't need to set this if the nim executable is inside your PATH."
  :type 'string
  :group 'nim)

(defcustom nim-args-compile '()
  "The arguments to pass to `nim-command' to compile a file."
  :type '(repeat string)
  :group 'nim)

(defcustom nim-compiled-buffer-name "*nim-js*"
  "The name of the scratch buffer used to compile Javascript from Nim."
  :type 'string
  :group 'nim)

(defun nim-compile (args &optional on-success)
  "Invoke the compiler and call ON-SUCCESS in case of successful compilation."
  (let ((on-success (or on-success (lambda () (message "Compilation successful.")))))
    (if (bufferp "*nim-compile*")
        (with-current-buffer "*nim-compile*"
          (erase-buffer)))))

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

(defun nim-compile-file-to-js (&optional callback)
  "Save current file and compiles it.
Use the project directory, so it will work best with external
libraries where `nim-compile-region-to-js' does not.  Return the
filename of the compiled file.  The CALLBACK is executed on
success with the filename of the compiled file."
  (interactive)
  (save-buffer)
  (let ((default-directory (or (nim-get-project-root) default-directory)))
    (nim-compile (list "js" (buffer-file-name))
                 (lambda () (when callback
                              (funcall callback (concat default-directory
                                                        "nimcache/"
                                                        (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
                                                        ".js")))))))

(defun nim-compile-region-to-js (start end)
  "Compile the current region to javascript.
The result is written into the buffer
`nim-compiled-buffer-name'."
  (interactive "r")

  (let ((buffer (get-buffer-create nim-compiled-buffer-name))
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


(require 'compile)
(add-to-list 'compilation-error-regexp-alist 'nim)
(add-to-list 'compilation-error-regexp-alist-alist
             '(nim "^\\s-*\\(.*\\)(\\([0-9]+\\),\\s-*\\([0-9]+\\))\\s-+\\(?:Error\\|\\(Hint\\)\\):" 1 2 3 (4)))

(provide 'nim-compile)
;;; nim-compile.el ends here
