;;; nim-suggest.el --- a plugin to use nimsuggest from Emacs -*- lexical-binding: t -*-
;;; Commentary:

;;

;;; Code:

(require 'nim-vars)
(require 'epc)
(require 'cl-lib)
(require 'nim-compile)
(require 'etags)
(require 'xref nil t)

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

(defun nimsuggest-get-options (project-path)
  "Get prerequisite options for EPC mode.

PROJECT-PATH is added as the last option."
  (delq nil
        (append nim-suggest-options nim-suggest-local-options
                ;; FIXME:
                ;; In recent nim’s update, this configuration no
                ;; longer can use.
                ;; (when (eq 'nimscript-mode major-mode)
                ;;   '("--define:nimscript" "--define:nimconfig"))
                (list (with-no-warnings nimsuggest-vervosity)
                      "--epc" project-path))))

(defun nim-find-project-path ()
  "Return project-path."
  ;; project-path is something like default directory, which nimsuggest treats.
  (or (and (eq 'nimscript-mode major-mode)
           buffer-file-name)
      (nim-find-config-file)
      buffer-file-name))

(defun nim-find-or-create-epc ()
  "Get the epc responsible for the current buffer."
  (let ((ppath (nim-find-project-path)))
    (or (let ((epc-process (cdr (assoc ppath nim-epc-processes-alist))))
          (if (eq 'run (epc:manager-status-server-process epc-process))
              epc-process
            (prog1 ()
              (nim-suggest-kill-zombie-processes ppath))))
        (let ((epc-process
               (epc:start-epc
                nim-nimsuggest-path
                (nimsuggest-get-options ppath))))
          (push (cons ppath epc-process) nim-epc-processes-alist)
          epc-process))))

;;;###autoload
(defun nim-suggest-available-p ()
  "Return non-nil if nimsuggest is available in current buffer."
  (and nim-nimsuggest-path
       (not nim-inside-compiler-dir-p)
       ;; Prevent turn on nimsuggest related feature on org-src block
       ;; or nimscript-mode (nimsuggest doesn't support yet).
       ;; https://github.com/nim-lang/nimsuggest/issues/29
       (not (memq major-mode '(org-mode nimscript-mode)))
       (not (and (fboundp 'org-in-src-block-p)
                 (or (org-in-src-block-p)
                     (org-in-src-block-p t))))))

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
  (when (nim-suggest-available-p)
    (let ((tempfile (nim-save-buffer-temporarly))
          (buf (current-buffer)))
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
          (lambda (_)
            (unless (get-buffer buf)
              (delete-file tempfile))))
        (deferred:error it
          (lambda (err)
            (message "%s" (error-message-string err))))))))

(defvar nim-dirty-directory
  ;; Even users changed the temp directory name,
  ;; ‘file-name-as-directory’ ensures suffix directory separator.
  (mapconcat 'file-name-as-directory
             `(,temporary-file-directory "emacs-nim-mode") "")
  "Directory name, which nimsuggest uses temporarily.
Note that this directory is removed when you exit from Emacs.")

(defun nimsuggest--get-dirty-dir ()
  "Return temp directory.
The directory name consists of `nim-dirty-directory' and current
frame number.  The frame number is required to prevent Emacs
crash when some emacsclients open the same file."
  (let* ((frame-num (nth 2 (split-string (format "%s" (selected-frame)) " ")))
         (frame-num-str (substring frame-num 0 (1- (length frame-num)))))
    (file-name-as-directory (concat nim-dirty-directory frame-num-str))))

(defun nim-suggest-get-temp-file-name ()
  "Get temp file name."
  (mapconcat 'directory-file-name
             `(,(nimsuggest--get-dirty-dir)
               ,(cl-case system-type
                  ((ms-dos windows-nt cygwin)
                   ;; For bug #119, convert ":" to "꞉" (U+A789)
                   (concat "/"
                           (replace-regexp-in-string
                            ":" (char-to-string #xA789)
                            buffer-file-name)))
                  (t ; for *nix system
                   buffer-file-name)))
             ""))

(defun nim-make-tempdir (tempfile)
  "Make temporary directory for TEMPFILE."
  (let* ((tempdir (file-name-directory tempfile)))
    (unless (file-exists-p tempdir)
      (make-directory tempdir t))))

(defun nim-save-buffer-temporarly ()
  "Save the current buffer and return the location."
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

(defun nim-suggest-kill-zombie-processes (&optional ppath)
  "Kill needless zombie processes, which correspond to PPATH."
  (setq nim-epc-processes-alist
        (cl-loop for (file . manager) in nim-epc-processes-alist
                 if (and (epc:live-p manager)
                         (or (and ppath (equal ppath file))
                             (not ppath)))
                 collect (cons file manager)
                 else do (epc:stop-epc manager))))

(defun nimsuggest-find-definition ()
  "Go to the definition of the symbol currently under the cursor."
  (interactive)
  (nim-call-epc
   'def
   (lambda (defs)
     (let ((def (cl-first defs)))
       (when (not def) (error "Definition not found"))
       (if (fboundp 'xref-push-marker-stack)
           (xref-push-marker-stack)
         (with-no-warnings
           (ring-insert find-tag-marker-ring (point-marker))))
       (find-file (nim-epc-filePath def))
       (goto-char (point-min))
       (forward-line (1- (nim-epc-line def)))))))
(define-obsolete-function-alias 'nim-goto-sym 'nimsuggest-find-definition "2017/9/02")

;; To avoid warning
(autoload 'flycheck-nimsuggest-setup "flycheck-nimsuggest")

(defvar nimsuggest-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.") #'nimsuggest-find-definition)
    (define-key map (kbd "M-,") #'pop-tag-mark)
    map))

;;;###autoload
(define-minor-mode nimsuggest-mode
  "Minor mode for nimsuggest."
  :lighter " nimsuggest"
  :keymap nimsuggest-mode-map
  (if nimsuggest-mode
      ;; Turn on
      (when (and (derived-mode-p 'nim-mode) (nim-suggest-available-p))
        ;; EL-DOC
        (when (or (bound-and-true-p eldoc-mode)
                  (bound-and-true-p global-eldoc-mode))
          (add-function :before-until (local 'eldoc-documentation-function)
                        'nim-eldoc-function))
        ;; flycheck
        (flycheck-nimsuggest-setup))
    ;; Turn off
    ;; FIXME: find proper way to turn off flycheck
    (remove-function (local 'eldoc-documentation-function) 'nim-eldoc-function)))

(provide 'nim-suggest)
;;; nim-suggest.el ends here
