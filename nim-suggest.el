;;; nim-suggest.el --- a plugin to use nimsuggest from Emacs -*- lexical-binding: t -*-

;;; Commentary:

;; memo
;; https://irclogs.nim-lang.org/12-07-2017.html

;;; Code:

(require 'nim-vars)
(require 'epc)
(require 'cl-lib)
(require 'nim-compile)

;;; If you change the order here, make sure to change it over in
;;; nimsuggest.nim too.
(defconst nimsuggest--epc-order
  '(:section :symkind :qualifiedPath :filePath :forth :line :column :doc :quality))

(cl-defstruct nimsuggest--epc
  section symkind qualifiedPath filePath forth line column doc quality)

(defun nimsuggest--parse-epc (obj method)
  "Parse OBJ according to METHOD."
  (cl-case method
    ((chk highlight outline) obj)
    ((sug con def use dus)
     (cl-mapcar
      (lambda (sublist)
        (apply #'make-nimsuggest--epc
               (cl-mapcan #'list nimsuggest--epc-order sublist)))
      obj))))

(defvar nimsuggest--epc-processes-alist nil)

(defvar nimsuggest-get-option-function nil
  "Function to get options for nimsuggest.")

(defun nimsuggest-get-options (project-path)
  "Get prerequisite options for EPC mode.

PROJECT-PATH is added as the last option."
  (delq nil
        (append nimsuggest-options nimsuggest-local-options
                ;; FIXME:
                ;; In recent nim’s update, this configuration no
                ;; longer can use.
                ;; (when (eq 'nimscript-mode major-mode)
                ;;   '("--define:nimscript" "--define:nimconfig"))
                (list "--epc" project-path))))

(defun nimsuggest--find-or-create-epc ()
  "Get the epc responsible for the current buffer."
  (let ((file buffer-file-name))
    (or (let ((old-epc (cdr (assoc file nimsuggest--epc-processes-alist))))
          (if (eq 'run (epc:manager-status-server-process old-epc))
              (prog1 old-epc
                (nim-log "nimsuggest: use old EPC process\n - %s" old-epc))
            (prog1 () (nimsuggest--kill-zombie-processes file))))
        (let ((new-epc
               (epc:start-epc
                nimsuggest-path
                (nimsuggest-get-options file))))
          (nim-log "nimsuggest: new EPC process created\n - %s" new-epc)
          (push (cons file new-epc) nimsuggest--epc-processes-alist)
          new-epc))))

;;;###autoload
(defun nimsuggest-available-p ()
  "Return non-nil if nimsuggest is available in current buffer."
  (and nimsuggest-path
       (not nim--inside-compiler-dir-p)
       ;; Prevent turn on nimsuggest related feature on org-src block
       ;; or nimscript-mode (nimsuggest doesn't support yet).
       ;; https://github.com/nim-lang/nimsuggest/issues/29
       (not (memq major-mode '(org-mode nimscript-mode)))
       (not (and (fboundp 'org-in-src-block-p)
                 (or (org-in-src-block-p)
                     (org-in-src-block-p t))))))
(define-obsolete-function-alias 'nim-suggest-available-p 'nimsuggest-available-p "2017/9/02")

(defun nimsuggest--call-epc (method callback)
  "Call the nimsuggest process on point.

Call the nimsuggest process responsible for the current buffer.
All commands work with the current cursor position.  METHOD can be
one of:

sug: suggest a symbol
con: suggest, but called at fun(_ <-
def: where the symbol is defined
use: where the symbol is used
dus: def + use

The CALLBACK is called with a list of ‘nimsuggest--epc’ structs.

REPORT-FN is for `flymake'.  See `flymake-diagnostic-functions'"
  (when (nimsuggest-available-p)
    ;; See also compiler/modulegraphs.nim for dirty file
    (let ((temp-dirty-file (nimsuggest--save-buffer-temporarly))
          (buf (current-buffer)))
      (deferred:$
        (epc:call-deferred
         (nimsuggest--find-or-create-epc)
         (prog1 method
           (nim-log "EPC-1 %S" (symbol-name method)))
         (cl-case method
           ((chk highlight outline)
            (list (buffer-file-name)
                  -1 -1
                  temp-dirty-file))
           (t
            (list (buffer-file-name)
                  (line-number-at-pos)
                  (current-column)
                  temp-dirty-file))))
        (deferred:nextc it
          (lambda (x)
            (nim-log "EPC(%S) nextc" (symbol-name method))
            (funcall callback (nimsuggest--parse-epc x method))))
        (deferred:watch it
          (lambda (_)
            (unless (get-buffer buf)
              (nim-log "EPC(%S) delete %s" (symbol-name method) buf)
              (delete-file temp-dirty-file))))
        (deferred:error it
          (lambda (err)
            (nim-log-err "EPC error %s" (error-message-string err))))))))

(defun nimsuggest--call-sync (method callback)
  (let* ((buf (current-buffer))
         (start (time-to-seconds))
         (res 'trash))
    (nimsuggest--call-epc
     method
     (lambda (candidates)
       (when (eq (current-buffer) buf)
         (setq res (funcall callback candidates)))))
    (while (and (eq 'trash res) (eq (current-buffer) buf))
      (if (> (- (time-to-seconds) start) 2)
          (nim-log "EPC-sync(%s): timeout %d sec" (symbol-name method) 2)
        (sleep-for 0.03)))
    (unless (eq 'trash res)
      res)))

(defun nimsuggest--get-dirty-dir ()
  "Return temp directory.
The directory name consists of `nimsuggest-dirty-directory' and current
frame number.  The frame number is required to prevent Emacs
crash when some emacsclients open the same file."
  (let* ((frame-num (nth 2 (split-string (format "%s" (selected-frame)) " ")))
         (frame-num-str (substring frame-num 0 (1- (length frame-num)))))
    (file-name-as-directory (concat nimsuggest-dirty-directory frame-num-str))))

(defun nimsuggest--get-temp-file-name ()
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

(defun nimsuggest--make-tempdir (tempfile)
  "Make temporary directory for TEMPFILE."
  (let* ((tempdir (file-name-directory tempfile)))
    (unless (file-exists-p tempdir)
      (make-directory tempdir t))))

(defun nimsuggest--save-buffer-temporarly ()
  "Save the current buffer and return the location."
  (let* ((temporary-file-directory nimsuggest-dirty-directory)
         (filename (nimsuggest--get-temp-file-name)))
    (nimsuggest--make-tempdir filename)
    (save-restriction
      (widen)
      (write-region (point-min) (point-max) filename nil 1))
    filename))

(add-hook 'kill-emacs-hook 'nimsugget--delete-temp-directory)
(defun nimsugget--delete-temp-directory ()
  "Delete temporary files directory for nimsuggest."
  (when (file-exists-p nimsuggest-dirty-directory)
    (delete-directory (file-name-directory nimsuggest-dirty-directory) t)))

(defun nimsuggest--kill-zombie-processes (&optional ppath)
  "Kill needless zombie processes, which correspond to PPATH."
  (setq nimsuggest--epc-processes-alist
        (cl-loop for (file . manager) in nimsuggest--epc-processes-alist
                 if (and (epc:live-p manager)
                         (or (and ppath (equal ppath file))
                             (not ppath)))
                 collect (cons file manager)
                 else do (epc:stop-epc manager))))


;; To avoid warning
(autoload 'flycheck-nimsuggest-setup "flycheck-nimsuggest")

(defvar nimsuggest-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-d") #'nimsuggest-show-doc)
    map))

(defcustom nimsuggest-mode-hook nil
  "Hook run when entering Nimsuggest mode."
  :options '(flycheck-nimsuggest-setup nimsuggest-flymake-setup nimsuggest-xref)
  :type 'hook
  :group 'nim)

;;;###autoload
(define-minor-mode nimsuggest-mode
  "Minor mode for nimsuggest."
  :lighter " nimsuggest"
  :keymap nimsuggest-mode-map
  (when nimsuggest-mode
    (nimsuggest-ensure)))

(defun nimsuggest-force-stop ()
  "Try to stop nimsuggest related things, but not well tested."
  (interactive)
  (remove-hook 'flycheck-checkers 'nim-nimsuggest)
  (remove-hook 'flymake-diagnostic-functions 'flymake-nimsuggest t)
  (nim-eldoc-off)
  (nimsuggest-xref 'off))

(defun nimsuggest-ensure ()
  "Ensure that users installed nimsuggest executable."
  ;; I've seen so many people just stacked to install nimsuggest at
  ;; first time. Probably this package's name is kinda confusing.
  (interactive)
  (let ((msg "Nimsuggest-mode needs external tool called nimsuggest.
Generally you can build by './koch tools' or '.koch nimsuggest'
on Nim repo (check koch.nim file), but it's good to check README
on Nim's official repository on yourself in case this document
was outdated."))
    (when (not nimsuggest-path)
      (nimsuggest-force-stop)
      (error msg))
    (when (not (file-executable-p nimsuggest-path))
      (nimsuggest-force-stop)
      (error "`nimsuggest-path' isn't executable; %s" msg))
    (if nimsuggest-mode
        (nim-log "nimsuggest-mode started")
      (nim-log "nimsuggest-mode stopped"))))


;; Utilities

(defun nimsuggest--put-face (text face)
  (when (and text (string< "" text))
    (add-text-properties
     0 (length text)
     `(face ,face)
     text)))

(defun nimsuggest--parse (forth)
  (when (string-match
         (rx (group (1+ word)) (0+ " ")
             (group (1+ nonl)))
         forth)
    (let ((first (match-string 1 forth))
          (other (match-string 2 forth)))
      (cons first other))))

(defun nimsuggest--trim (str)
  "Adjust STR for mini buffer."
  (let ((max-width (- (frame-width) 4))) ; <- just for buffer, probably
    (if (< (length str) max-width)       ; it depends on terminal or GUI Emacs
        str
      (let* ((short-str (substring str 0 (- (frame-width) 4)))
             (minus-offset
              (cl-loop with num = 0
                       for s in (delq "" (split-string (reverse short-str) ""))
                       if (equal s ".") do (cl-return num)
                       else do (cl-incf num)
                       finally return 0)))
        (substring short-str 0 (- (length short-str) minus-offset))))))

(defun nimsuggest--format (forth symKind qpath doc)
  "Highlight returned result from nimsuggest."
  (let* ((doc (mapconcat 'identity (split-string doc "\n") ""))
         (name
          (if (eq (length (cdr qpath)) 1)
              (cadr qpath)
            (mapconcat 'identity (cdr qpath) "."))))
    (nimsuggest--put-face doc font-lock-doc-face)
    (pcase (list symKind)
      (`(,(or "skProc" "skField" "skTemplate" "skMacro"))
       (when (string< "" forth)
         (cl-destructuring-bind (ptype . typeinfo) (nimsuggest--parse forth)
           (when (equal "proc" ptype)
             (nimsuggest--put-face name font-lock-function-name-face)
             (let* ((func  (format "%s %s" name typeinfo)))
               (nimsuggest--trim
                (if (string= "" doc)
                    (format "%s" func)
                  (format "%s %s" func doc))))))))
      (`(,(or "skVar" "skLet" "skConst" "skResult" "skParam"))
       (let ((sym (downcase (substring symKind 2 (length symKind)))))
         (nimsuggest--put-face sym font-lock-keyword-face)
         (nimsuggest--put-face name
                             (cond ((member symKind '("skVar" "skResult"))
                                    '(face font-lock-variable-name-face))
                                   ((member symKind '("skLet" "skConst"))
                                    '(face font-lock-constant-face))
                                   (t '(face font-lock-keyword-face))))
         (nimsuggest--trim
          (format "%s %s : %s" sym name
                  (cond
                   ((string< "" forth) forth)
                   (t "no doc"))))))
      (`("skType")
       (nimsuggest--put-face name font-lock-type-face)
       (nimsuggest--trim
        (if (not (string< "" doc))
            (format "%s: no doc" name)
          (format "%s: %s" name doc)))))))


;;; misc

;; work in progress

(defcustom nimsuggest-doc-directive
  'def
  "Directive passed by nimsuggest for `nimsuggest-show-doc'."
  :type '(choice
          (const :tag "suggest" 'sug)
          (const :tag "definition" 'def))
  :group 'nim)

(defvar nimsuggest--doc-args nil
  "Internal variable to store document data.")

(defun nimsuggest-show-doc ()
  "Show document in dedicated *nim-doc* buffer."
  (interactive)
  (nimsuggest--call-epc
   nimsuggest-doc-directive
   (lambda (args)
     (if (and (not args) (not (eq 'sug nimsuggest-doc-directive)))
         ;; Fallback if there is no result from nimsuggest by 'sug
         (let ((nimsuggest-doc-directive 'sug))
           (nimsuggest-show-doc))
       ;; TODO: should I filter returned result by current position's identifier?
       (setq nimsuggest--doc-args (cl-loop for i from 0 to (1- (length args))
                                           collect (cons (1+ i) (nth i args))))
       (nimsuggest--show-doc)))))

(defun nimsuggest--show-doc ()
  (let ((def (cdar nimsuggest--doc-args)))
    (get-buffer-create "*nim-doc*")
    (unless (equal (current-buffer) (get-buffer "*nim-doc*"))
      (switch-to-buffer-other-window "*nim-doc*"))
    (setq buffer-read-only nil)
    (erase-buffer)
    (cl-loop for str in (list
                         ;; (format "debug %s\n" nimsuggest--doc-args)
                         (let ((nominator (caar nimsuggest--doc-args))
                               (denominator (length nimsuggest--doc-args)))
                           (format "%s %s\n"
                                   (mapconcat 'identity (nimsuggest--epc-qualifiedPath def) " ")
                                   (if (eq 1 denominator)
                                       ""
                                     (format "%s/%s %s" nominator denominator
                                             "-- < next, > previous"))))
                         (format "Signature\n#########\n%s\n"
                                 (format "%s %s"
                                         (nimsuggest--epc-symkind def)
                                         (nimsuggest--epc-forth def)))
                         (format "Document\n########\n%s\n"
                                 (nimsuggest--epc-doc def))
                         (format "Location\n########\n%s\n"
                                 (nimsuggest--epc-filePath def)))
             do (insert (concat str "\n")))
    ;; For highlight stuff
    (when (fboundp 'rst-mode) (rst-mode))
    (goto-char (point-min))
    (use-local-map nimsuggest-doc-mode-map)
    (setq buffer-read-only t)))

(defun nimsuggest-doc-next ()
  "Move to next page."
  (interactive)
  (if (not (< 0 (length nimsuggest--doc-args)))
      (minibuffer-message "there is no next")
    (let ((popped (pop nimsuggest--doc-args)))
      (setq nimsuggest--doc-args (append nimsuggest--doc-args (list popped)))
      (nimsuggest--show-doc))))

(defun nimsuggest-doc-previous ()
  "Move to previous page."
  (interactive)
  (if (not (< 0 (length nimsuggest--doc-args)))
      (minibuffer-message "there is no previous")
    (let* ((rargs (reverse nimsuggest--doc-args))
           (popped (pop rargs)))
      (setq rargs (append rargs (list popped))
            nimsuggest--doc-args (reverse rargs))
      (nimsuggest--show-doc))))


;;; Flymake integration

;; From Emacs 26, flymake was re-written by João Távora.
;; It supports asynchronous backend, so enable it if users
;; turned on the flymake-mode.

;; Manual configuration:
;;   (add-hook 'nimsuggest-mode-hook 'nimsuggest-flymake-setup)

(if (version<= "26" (number-to-string emacs-major-version))
    (add-hook 'nimsuggest-mode-hook 'nimsuggest-flymake-setup)
  (add-hook 'nimsuggest-mode-hook 'flycheck-nimsuggest-setup))

;;;###autoload
(defun nimsuggest-flymake-setup()
  "Kinda experimental function to use flymake on Emacs 26."
  (when (and (bound-and-true-p flymake-mode)
             (not (bound-and-true-p flycheck-mode)))
    (if nimsuggest-mode
        (add-hook  'flymake-diagnostic-functions 'flymake-nimsuggest nil t)
      (remove-hook 'flymake-diagnostic-functions 'flymake-nimsuggest t))))

(defun nimsuggest--flymake-error-parser (errors buffer)
  "Return list of result of `flymake-make-diagnostic' from ERRORS.
The list can be nil.  ERRORS will be skipped if BUFFER and
parsed file was different."
  (cl-loop for (_ _ _ file typ line col text _) in errors
           for type = (cl-case (string-to-char typ)
                        (?E :error)
                        (?W :warning)
                        (t  :note))
           ;; nimsuggest's column starts from 1, but Emacs is 0.
           ;; Use funcall to circumvent emacs' not defined warning
           for (beg . end) = (funcall 'flymake-diag-region buffer line (1+ col))
           if (eq buffer (get-file-buffer file))
           collect (funcall 'flymake-make-diagnostic buffer beg end type text)))

(defun flymake-nimsuggest (report-fn &rest _args)
  "A Flymake backend for Nim language using Nimsuggest.
See `flymake-diagnostic-functions' for REPORT-FN and ARGS."
  (let ((buffer (current-buffer)))
    (nimsuggest--call-epc
     'chk
     (lambda (errors)
       (nim-log "FLYMAKE(OK): report(s) number of %i" (length errors))
       (condition-case err
           (let ((report-action
                  (nimsuggest--flymake-error-parser errors buffer)))
             (funcall report-fn (delq nil report-action)))
         (error
          (nimsuggest-flymake--panic report-fn (error-message-string err))))))))

;; TODO: not sure where to use this yet... Using this function cause
;; to stop flymake completely which is not suitable for nimsuggest
;; because nimsuggest re-start after its crush.
(defun nimsuggest-flymake--panic (report-fn err)
  (when (member 'flymake-nimsuggest flymake-diagnostic-functions)
    (nim-log-err "FLYMAKE(ERR): %s" err)
    (funcall report-fn :panic :explanation err)))


;;; ElDoc for nimsuggest

(defvar nimsuggest-eldoc--data nil)

;;;###autoload
(defun nimsuggest-eldoc--nimsuggest ()
  (when (nimsuggest-available-p)
    (unless (nimsuggest-eldoc--same-try-p)
      (nimsuggest-eldoc--call))
    (when (eq (line-number-at-pos)
              (assoc-default :line nimsuggest-eldoc--data))
      (assoc-default :str nimsuggest-eldoc--data))))

(defun nimsuggest-eldoc--same-try-p ()
  (or (and (equal (nim-current-symbol)
                  (assoc-default :name nimsuggest-eldoc--data))
           (eq (assoc-default :line nimsuggest-eldoc--data)
               (line-number-at-pos)))
      (and (nim-eldoc-inside-paren-p)
           (save-excursion
             (nimsuggest-eldoc--move)
             (or
              ;; for template
              (eq (point) (assoc-default :pos nimsuggest-eldoc--data))
              ;; for proc
              (eq (1- (point)) (assoc-default :pos nimsuggest-eldoc--data)))))))

(defun nimsuggest-eldoc--move ()
  (let ((pos  (point))
        (ppss (syntax-ppss)))
    (when (nim-eldoc-inside-paren-p)
      (goto-char (nth 1 ppss))
      (when (looking-back nim-eldoc--skip-regex nil)
        (goto-char pos)))))

(defun nim-eldoc-format-string (defs)
  "Format data inside DEFS for eldoc.
DEFS is group of definitions from nimsuggest."
  ;; TODO: switch if there are multiple defs
  (nim-log "ELDOC format")
  (let* ((data    (cl-first defs))
         (forth   (nimsuggest--epc-forth         data))
         (symKind (nimsuggest--epc-symkind       data))
         (qpath   (nimsuggest--epc-qualifiedPath data))
         (doc     (nimsuggest--epc-doc           data)))
    (nimsuggest--format forth symKind qpath doc)))

(defun nimsuggest-eldoc--call ()
  (save-excursion
    (nimsuggest-eldoc--move)
    (nim-log "ELDOC-1")
    (nimsuggest--call-epc
     ;; version 2 protocol can use: ideDef, ideUse, ideDus
     'dus 'nimsuggest-eldoc--update)))

(defun nimsuggest-eldoc--update (defs)
  (if (nim-eldoc--on-string-p)
      (nim-log "ELDOC stop update")
    (nim-log "ELDOC update")
    (if defs
        (nimsuggest-eldoc--update-1 defs)
      (save-excursion
        (when (nim-eldoc-inside-paren-p)
          (nimsuggest-eldoc--move)
          (backward-char)
          (nimsuggest--call-epc 'dus 'nimsuggest-eldoc--update-1))))))

(defun nimsuggest-eldoc--update-1 (defs)
  (when defs
    (setq nimsuggest-eldoc--data
          (list
           (cons :str  (nim-eldoc-format-string defs))
           (cons :line (line-number-at-pos))
           (cons :name (nim-current-symbol))
           (cons :pos  (point))))
    (setq eldoc-last-message (assoc-default :str nimsuggest-eldoc--data))
    (message eldoc-last-message)))


;;; xref integration
;; This package likely be supported on Emacs 25.1 or later
(eval-after-load "xref"
  '(progn
     (defun nimsuggest--xref-backend () 'nimsuggest)
     (defun nimsuggest-xref (&optional on-or-off)
       (nim-log "xref status: %s" on-or-off)
       (cl-case on-or-off
         (on  (add-hook 'xref-backend-functions #'nimsuggest--xref-backend nil t))
         (off (remove-hook 'xref-backend-functions #'nimsuggest--xref-backend t))
         (t (when on-or-off
              (nimsuggest-xref (if nimsuggest-mode 'on 'off))))))

     (add-hook 'nimsuggest-mode-hook 'nimsuggest-xref)

     (cl-defmethod xref-backend-identifier-at-point ((_backend (eql nimsuggest)))
       "Return string or nil for identifier at point."
       ;; Well this function may not needed for current xref functions for
       ;; nimsuggest backend.
       (with-syntax-table nim-dotty-syntax-table
         (let ((thing (thing-at-point 'symbol)))
           (and thing (substring-no-properties thing)))))

     (defun nimsuggest--xref-make-obj (id def)
       (let ((summary id)
             (location (xref-make-file-location
                        (nimsuggest--epc-filePath def)
                        (nimsuggest--epc-line def)
                        (nimsuggest--epc-column def))))
         (xref-make summary location)))

     (defun nimsuggest--xref (query id)
       (nimsuggest--call-sync
        query
        (lambda (results)
          (cond
           ((null results) nil)
           ((listp results)
            (cl-loop for result in results
                     collect (nimsuggest--xref-make-obj id result)))))))

     (cl-defmethod xref-backend-definitions ((_backend (eql nimsuggest)) id)
       (nimsuggest--xref 'def id))

     (cl-defmethod xref-backend-references ((_backend (eql nimsuggest)) id)
       (nimsuggest--xref 'dus id))

     ;; just define empty backend to use `xref-backend-references' for
     ;; nimsuggest.
     (cl-defmethod xref-backend-identifier-completion-table
       ((_backend (eql nimsuggest))))

     ;; Not implement yet, or not sure maybe, won't...
     ;; (cl-defmethod xref-backend-apropos ((_backend (eql nimsuggest)) pattern))

     )) ; end of eval-after-load xref

;; Work around for old Emacsen
(if (fboundp 'xref-find-definitions)
    (defun nimsuggest-find-definition (id)
      "Go to the definition of the symbol currently under the cursor.
This uses `xref-find-definitions' as backend."
      (interactive (list (xref--read-identifier "Find definitions of: ")))
      (xref-find-definitions id))

  ;; Note below configuration were removed on the future
  (define-key nimsuggest-mode-map (kbd "M-.") #'nimsuggest-find-definition)
  (define-key nimsuggest-mode-map (kbd "M-,") #'pop-tag-mark)
  (require 'etags)
  (defun nimsuggest-find-definition (&optional _id)
    "Go to the definition of the symbol currently under the cursor."
    (nimsuggest--call-epc
     'def
     (lambda (defs)
       (let ((def (cl-first defs)))
         (when (not def) (error "Definition not found"))
         (if (fboundp 'xref-push-marker-stack)
             (xref-push-marker-stack)
           (with-no-warnings
             (ring-insert find-tag-marker-ring (point-marker))))
         (find-file (nimsuggest--epc-filePath def))
         (goto-char (point-min))
         (forward-line (1- (nimsuggest--epc-line def))))))))

(define-obsolete-function-alias 'nim-goto-sym 'nimsuggest-find-definition
  "2017/9/02")


(provide 'nim-suggest)
;;; nim-suggest.el ends here
