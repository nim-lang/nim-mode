;;; nimrod-mode.el --- A major mode for the Nimrod programming language
;;
;; Filename: nimrod-mode.el
;; Description: A major mode for the Nimrod programming language
;; Author: Simon Hafner
;; Maintainer: Simon Hafner <hafnersimon@gmail.com>
;; Version: 0.1.5
;; Keywords: nimrod
;; Compatibility: GNU Emacs 24
;; Package-Requires: ((auto-complete "1.4"))
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

(require 'auto-complete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Helpers                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nimrod-glue-strings (glue strings)
  "Given a list of strings and some glue, concatenate."
  (mapconcat 'identity strings glue))

(defun nimrod-regexp-choice (strings)
  "Given a list of strings, construct a regexp multiple-choice."
  (concat "\\(" (nimrod-glue-strings "\\|" strings) "\\)"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Simple keywords                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Define keywords, etc.
;; ---------------------

(defvar nimrod-keywords
  (split-string "
addr and as asm atomic
bind block break
case cast const continue converter
discard distinct div do
elif else end enum except export
finally for from
generic
if import in include interface is isnot iterator
lambda let
macro method mixin mod
nil not notin
object of or out
proc ptr
raise ref return
shared shl shr static
template try tuple type
var
when while with without
xor
yield
")
  "Nimrod keywords. The above string is taken from
<http://force7.de/nimrod/manual.html#identifiers-keywords>,
for easy updating.")

(defvar nimrod-types
  '("int" "int8" "int16" "int32" "int64" "float" "float32" "float64"
    "bool" "char" "string" "cstring" "pointer" "ordinal" "nil" "expr"
    "stmt" "typedesc" "range" "array" "openarray" "seq" "set"
    "tgenericseq" "pgenericseq" "nimstringdesc" "nimstring" "byte"
    "natural" "positive" "tobject" "pobject" "tresult" "tendian"
    "taddress" "biggestint" "biggestfloat" "cchar" "cschar" "cshort"
    "cint" "clong" "clonglong" "cfloat" "cdouble" "clongdouble"
    "cstringarray" "pfloat32" "pfloat64" "pint64" "pint32"
    "tgc_strategy" "tfile" "tfilemode")
  "Nimrod types defined in <lib/system.nim>."

  )

(defvar nimrod-exceptions
  '("e_base" "easynch" "esynch" "esystem" "eio" "eos"
    "einvalidlibrary" "eresourceexhausted" "earithmetic" "edivbyzero"
    "eoverflow" "eaccessviolation" "eassertionfailed" "econtrolc"
    "einvalidvalue" "eoutofmemory" "einvalidindex" "einvalidfield"
    "eoutofrange" "estackoverflow" "enoexceptiontoreraise"
    "einvalidobjectassignment" "einvalidobjectconversion"
    "efloatingpoint" "efloatinginvalidop" "efloatdivbyzero"
    "efloatoverflow" "efloatunderflow" "efloatinexact")
  "Nimrod exceptions defined in <lib/system.nim>.")

(defvar nimrod-constants
  '("ismainmodule" "compiledate" "compiletime" "nimrodversion"
    "nimrodmajor" "nimrodminor" "nimrodpatch" "cpuendian" "hostos"
    "hostcpu" "apptype" "inf" "neginf" "nan" "quitsuccess"
    "quitfailure" "stdin" "stdout" "stderr" "true" "false" )
  "Nimrod constants defined in <lib/system.nim>.")

(defvar nimrod-builtins
  '("defined" "definedinscope" "not" "+" "-" "=" "<" ">" "@" "&" "*"
    ">=" "<=" "$" ">=%" ">%" "<%" "<=%" "," ":" "==" "/"  "div" "mod"
    "shr" "shl" "and" "or" "xor" "abs" "+%" "-%" "*%" "/%" "%%" "-+-"
    "not_in" "is_not" "cmp" "high" "low" "sizeof" "succ" "pred" "inc"
    "dec" "newseq" "len" "incl" "excl" "card" "ord" "chr" "ze" "ze64"
    "tou8" "tou16" "tou32" "min" "max" "setlen" "newstring" "add"
    "compileoption" "del" "delete" "insert" "repr" "tofloat"
    "tobiggestfloat" "toint" "tobiggestint" "addquitproc" "copy"
    "zeromem" "copymem" "movemem" "equalmem" "alloc" "alloc0"
    "realloc" "dealloc" "assert" "swap" "getrefcount" "getoccupiedmem"
    "getfreemem" "gettotalmem" "countdown" "countup" "items"
    "enumerate" "isnil" "find" "contains" "pop" "each" "gc_disable"
    "gc_enable" "gc_fullcollect" "gc_setstrategy"
    "gc_enablemarkandsweep" "gc_disablemarkandsweep"
    "gc_getstatistics" "gc_ref" "gc_unref" "accumulateresult" "echo"
    "newexception" "quit" "open" "reopen" "close" "endoffile"
    "readchar" "flushfile" "readfile" "write" "readline" "writeln"
    "getfilesize" "readbytes" "readchars" "readbuffer" "writebytes"
    "writechars" "writebuffer" "setfilepos" "getfilepos" "lines"
    "filehandle" "cstringarraytoseq" "getdiscriminant" "selectbranch"
    "getcurrentexception" "getcurrentexceptionmsg" "likely" "unlikely"
    )
  "Standard library functions fundamental enough to count as builtins.
Magic functions."
  )

(defvar nimrod-operators
  '( "`" "{." ".}" "[" "]" "{" "}" "(" ")" )
  "Nimrod standard operators.")


;; Custom faces
;; ------------

;; TODO: make work!?
(defface nimrod-tab-face
  '((((class color) (background dark))
     (:background "grey22" :foreground "darkgray"))
    (((class color) (background light))
     (:background "beige"  :foreground "lightgray"))
    (t (:inverse-video t)))
  "Face used to visualize TAB."
  :group 'whitespace)


;; Create regular expressions
;; --------------------------


;; regexp-opt'ed expressions
;; '''''''''''''''''''''''''

(defvar nimrod-keywords-regexp (regexp-opt nimrod-keywords 'words))
(defvar nimrod-types-regexp (regexp-opt nimrod-types 'words))
(defvar nimrod-types-regexp (regexp-opt nimrod-exceptions 'words))
(defvar nimrod-constants-regexp (regexp-opt nimrod-constants 'words))
(defvar nimrod-builtins-regexp (regexp-opt nimrod-builtins 'words))
(defvar nimrod-operators-regexp (regexp-opt nimrod-operators 'words))

;; Free memory
(defvar nimrod-keywords nil)
(defvar nimrod-types nil)
(defvar nimrod-exceptions nil)
(defvar nimrod-constants nil)
(defvar nimrod-builtins nil)
(defvar nimrod-operators nil)


;; Hand-reared expressions
;; '''''''''''''''''''''''

(defvar nimrod-decimal-regexp
  "\\<[0-9_]+\\(\\.[0-9_]+\\)?\\([eE][0-9]+\\)?\\(\'\\(i8\\|i16\\|i32\\|i64\\|f32\\|f64\\)\\)?\\>"
  "Regular expression for matching decimal literals in Nimrod."
  )

(defvar nimrod-hex-regexp
  "\\<\\0x[0-9a-fA-F_]+\\(\\.[0-9a-fA-F_]+\\)?\\([eE][0-9a-fA-F]+\\)?\\(\'\\(i8\\|i16\\|i32\\|i64\\|f32\\|f64\\)\\)?\\>"
  "Regular expression for matching hexadecimal literals in Nimrod."
  )

(defvar nimrod-octal-regexp
  "\\<\\0o[0-7_]+\\(\\.[0-7_]+\\)?\\([eE][0-7]+\\)?\\(\'\\(i8\\|i16\\|i32\\|i64\\|f32\\|f64\\)\\)?\\>"
  "Regular expression for matching octal literals in Nimrod."
  )

(defvar nimrod-binary-regexp
  "\\<\\0b[01_]+\\(\\.[01_]+\\)?\\([eE][01]+\\)?\\(\'\\(i8\\|i16\\|i32\\|i64\\|f32\\|f64\\)\\)?\\>"
  "Regular expression for matching binary literals in Nimrod."
  )

(defvar nimrod-variables-regexp
  "\\<[a-zA-Z][a-zA-Z0-9_]+\\>"
  "Regular expression for matching variable identifiers in Nimrod."
  )

(defvar nimrod-character-literal-regexp
  "\\<\'\\(.\\|\\\\.*\\)'\\>"  ;; TODO: make more precise
  "Regular expression for matching character literal tokens."
  )

(defvar nimrod-single-quote-string-regexp
  "\\<\".*\"\\>"
  "Regular expression for matching single-quoted strings."
  )

(defvar nimrod-raw-string-regexp
  "\\<r\".*\"\\>"
  "Regular expression for matching raw strings."
  )

(defconst nimrod-tab-regexp "\\(\t+\\)")

(defconst nimrod-blank-line-regexp "^ *$"
  "Regexp matching a line containing only (valid) whitespace.")

(defconst nimrod-new-block-regexp
  (concat ".*"                                          ;; Anything
          (nimrod-regexp-choice '("=" "var" "type" "const" "enum" "\\:")) ;; ending in a new block indicator,
          " *"                                          ;; then non-syntactic whitespace,
          "\\(#.*\\)?"                                  ;; then possibly a comment.
          "$")
  "Regexp matching a line that precedes a new block.")


(setq nimrod-font-lock-keywords
      `(  ;; note the BACKTICK, `
        (,nimrod-raw-string-regexp . font-lock-string-face)
        (,nimrod-character-literal-regexp . font-lock-constant-face)
        (,nimrod-single-quote-string-regexp . font-lock-string-face)
        (,nimrod-tab-regexp . nimrod-tab-face) ;; TODO: make work!
        (,nimrod-keywords-regexp . font-lock-keyword-face)
        (,nimrod-types-regexp . font-lock-type-face)
        (,nimrod-constants-regexp . font-lock-constant-face)
        (,nimrod-builtins-regexp . font-lock-builtin-face)
        (,nimrod-decimal-regexp . font-lock-constant-face)
        (,nimrod-hex-regexp . font-lock-constant-face)
        (,nimrod-octal-regexp . font-lock-constant-face)
        (,nimrod-binary-regexp . font-lock-constant-face)
        (,nimrod-operators-regexp . font-lock-variable-name-face)
        (,nimrod-variables-regexp . font-lock-variable-name-face)
        ))

;; Free memory
(defvar nimrod-character-literal-regexp nil)
(defvar nimrod-raw-string-regexp nil)
(defvar nimrod-triple-quote-string-regexp nil)
(defvar nimrod-single-quote-string-regexp nil)
(defvar nimrod-tab-regexp nil)
(defvar nimrod-keywords-regexp nil)
(defvar nimrod-types-regexp nil)
(defvar nimrod-constants-regexp nil)
(defvar nimrod-builtins-regexp nil)
(defvar nimrod-decimal-regexp nil)
(defvar nimrod-hex-regexp nil)
(defvar nimrod-octal-regexp nil)
(defvar nimrod-binary-regexp nil)
(defvar nimrod-operators-regexp nil)
(defvar nimrod-variables-regexp nil)

(defun nimrod-setup-font-lock ()
  "This will be called when defining nimrod-node, below."
  (setq font-lock-defaults '((nimrod-font-lock-keywords))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Comments                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the command to comment/uncomment text
(defun nimrod-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let ((deactivate-mark nil) (comment-start "#") (comment-end ""))
    (comment-dwim arg)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Indentation                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Desired indentation logic:
;; 1. When a newline is entered, or <TAB> is pressed, do:
;;
;;    1. If this line is all comment (i.e. ^\w*#.*$ ),
;;
;;       1. Find previous non-blank line.
;;
;;       2. If contains comment, set expected indentation to there.
;;          Else, set to first non-whitespace character.
;;
;;    2. Else if this line starts as a string (i.e. ^\w*\".*$ ),
;;       Find expected indentation based on previous line,
;;       ignoring blank lines between:
;;

;; -- Indent to previous # character if we're on a commented line
;; -- Indent to previous start-of-string if line starts with string

;;; TODO: indent after object
;;; TODO: unindent after else:


(defconst nimrod-indent-offset 2 "Number of spaces per level of indentation.")

(defun nimrod-skip-blank-lines ()
  (progn
    (forward-line -1)                                  ;; Go back one line.
    (while (and (looking-at nimrod-blank-line-regexp)  ;; While it's a blank line,
                (> (point) (point-min)))               ;; and there are other lines,
      (forward-line -1))))                             ;; skip back.

(defun nimrod-compute-indentation-of-char (char)
  ""
  (progn
    (nimrod-skip-blank-lines)
    (skip-chars-forward (concat "^\n" char))
    (if (looking-at char)
        (current-column)
      (+ (progn
           (beginning-of-line)
           (if (looking-at nimrod-new-block-regexp) nimrod-indent-offset 0))
         (current-indentation)))))

(defun nimrod-compute-indentation ()
  "Calculate the maximum sensible indentation for the current line."
  (save-excursion
    (beginning-of-line)

    (cond ((looking-at "^ *#")  (nimrod-compute-indentation-of-char "#" )) ;; Comment line; look for a comment
          ((looking-at "^ *\"") (nimrod-compute-indentation-of-char "\"")) ;; String
          ((looking-at "^ *'")  (nimrod-compute-indentation-of-char "'" )) ;; Char
          (t                   (progn
                                 (nimrod-skip-blank-lines)
                                 (+ (current-indentation)
                                    (if (looking-at nimrod-new-block-regexp) nimrod-indent-offset 0)))))))


(defun nimrod-indent-line ()
  "Indent the current line.  The first time this command is used, the line
will be indented to the maximum sensible indentation.
Each immediately subsequent usage will back-dent the line by
`nimrod-indent-offset' spaces.
On reaching column 0, it will cycle back to the maximum sensible indentation."

  (interactive "*")

  (let ((ci (current-indentation))
        (cc (current-column))
        (need (nimrod-compute-indentation)))

    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      (if (and (equal last-command this-command) (/= ci 0))
          (indent-to (* (/ (- ci 1) nimrod-indent-offset) nimrod-indent-offset))
        (indent-to need)))

    (if (< (current-column) (current-indentation))
        (forward-to-indentation 0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Wrap it all up ...                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode nimrod-mode prog-mode
  "nimrod mode"
  "A major mode for the Nimrod programming language."

  (setq mode-name "Nimrod")  ;; This will display in the mode line.

  (nimrod-setup-font-lock)

  ;; modify the keymap
  (define-key nimrod-mode-map [remap comment-dwim] 'nimrod-comment-dwim)
  (define-key nimrod-mode-map (kbd "M-.") 'nimrod-goto-sym)
  (define-key nimrod-mode-map (kbd "C-c h") 'nimrod-explain-sym)

  (set (make-local-variable 'indent-line-function) 'nimrod-indent-line)

  ;; Documentation comment highlighting
  ;; (modify-syntax-entry ?\# ". 12b" nimrod-mode-syntax-table)
  ;; (modify-syntax-entry ?\n "> b" nimrod-mode-syntax-table)

  ;; Comment highlighting
  (modify-syntax-entry ?# "< b"  nimrod-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" nimrod-mode-syntax-table)

  (modify-syntax-entry ?\' "w"  nimrod-mode-syntax-table)
  (modify-syntax-entry ?\" "|"  nimrod-mode-syntax-table)

  (modify-syntax-entry ?\[ "("  nimrod-mode-syntax-table)
  (modify-syntax-entry ?\] ")"  nimrod-mode-syntax-table)

  (setq indent-tabs-mode nil) ;; Always indent with SPACES!
)

(defcustom nimrod-compiled-buffer-name "*nimrod-js*"
  "The name of the scratch buffer used to compile Javascript from Nimrod."
  :type 'string
  :group 'nimrod)

(defcustom nimrod-command "nimrod"
  "Path to the nimrod executable. If nimrod is in your PATH,
there should be no problem."
  :type 'string
  :group 'nimrod)

(defcustom nimrod-args-compile '()
  "The arguments to pass to `nimrod-command' to compile a file."
  :type 'list
  :group 'nimrod)

(defcustom nimrod-type-abbrevs '(
                                 ("skProc" . "f")
                                 ("skIterator" . "i")
                                 ("skTemplate" . "T")
                                 ("skType" . "t")
                                 ("skMethod" . "f")
                                 ("skEnumField" . "e")
                                 ("skGenericParam" . "p")
                                 ("skParam" . "p")
                                 ("skModule" . "m")
                                 ("skConverter" . "C")
                                 ("skMacro" . "M")
                                 ("skField" . "F")
                                 ("skForVar" . "v")
                                 ("skVar" . "v")
                                 ("skLet" . "v")
                                 ("skLabel" . "l")
                                 ("skConst" . "c")
                                 ("skResult" . "r")
                                 )
  "Abbrevs for auto-complete."
  :type 'assoc
  :group 'nimrod)

(defvar nimrod-idetools-modes '(suggest def context usages)
  "Which modes are available to use with the idetools.")

(defun nimrod-compile-file-to-js (&optional callback)
  "Saves current file and compiles it. Uses the project
directory, so it will work best with external libraries where
`nimrod-compile-region-to-js` does not. Returns the filename of
the compiled file. he callback is executed on success with the
filename of the compiled file."
  (interactive)
  (save-buffer)
  (let ((default-directory (or (nimrod-get-project-root) default-directory)))
    (lexical-let ((callback callback))
      (nimrod-compile (list "js" (buffer-file-name))
                      (lambda () (when callback
                              (funcall callback (concat default-directory
                                                        "nimcache/"
                                                        (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
                                                        ".js"))))))))

(defun nimrod-compile-region-to-js (start end)
  "Compiles the current region to javascript into the buffer
`nimrod-compiled-buffer-name'."
  (interactive "r")

  (lexical-let ((buffer (get-buffer-create nimrod-compiled-buffer-name))
                (tmpdir (file-name-as-directory (make-temp-file "nimrod-compile" t))))
    (let ((default-directory tmpdir))
      (write-region start end "tmp.nim" nil 'foo)
      (with-current-buffer buffer
        (erase-buffer)
        (let ((default-directory tmpdir))
          (nimrod-compile '("js" "tmp.nim")
                          (lambda () (with-current-buffer buffer
                                  (insert-file
                                   (concat tmpdir (file-name-as-directory "nimcache") "tmp.js"))
                                  (display-buffer buffer)))))))))

(defun nimrod-compile (args &optional on-success)
  "Invokes the compiler and calls on-success in case of
successful compile."
  (lexical-let ((on-success (or on-success (lambda () (message "Compilation successful.")))))
    (set-process-sentinel
     (apply
      (apply-partially 'start-file-process "nimrod" "*nimrod-compile*" nimrod-command)
      (append nimrod-args-compile args))
     (lambda (process-name status)
       (cond ((string= status "finished\n")
              (when on-success
                (funcall on-success)))
             ((string= status "exited abnormally with code 1\n")
              (display-buffer "*nimrod-compile*"))
             (t (error status)))))))

(defun nimrod-ac-enable ()
  "Enable Autocompletion. Default settings. If you don't like
them, kick this hook with
 `(remove-hook 'nimrod-mode-hook 'nimrod-ac-enable)`
and write your own. I discurage using autostart, as the
completion candidates need to be loaded from outside emacs."
  (when (not (executable-find nimrod-command))
    (error "NimRod executable not found. Please customize nimrod-command"))

  (make-local-variable 'ac-sources)
  (setq ac-sources '(ac-source-nimrod-completions))

  (make-local-variable 'ac-use-comphist)
  (setq ac-use-comphist nil)

  (make-local-variable 'ac-use-quick-help)
  (setq ac-use-quick-help t)

  (make-local-variable 'ac-delete-dups)
  (setq ac-delete-dups nil)

  (make-local-variable 'ac-ignore-case)
  (setq ac-ignore-case t)

  (make-local-variable 'ac-auto-show-menu)
  (setq ac-auto-show-menu 0.5)

  (make-local-variable 'ac-auto-start)
  (setq ac-auto-start nil)

  (make-local-variable 'ac-trigger-key)
  (setq ac-trigger-key "TAB")

  (auto-complete-mode)
)

(add-hook 'nimrod-mode-hook 'nimrod-ac-enable)

;;; Some copy/paste from ensime.
(ac-define-source nimrod-completions
  '((candidates . (nimrod-ac-completion-candidates ac-prefix))
    (prefix . nimrod-ac-completion-prefix)
    (action . (lambda ()))                   ; TODO
    (requires . 0)
    ))

(defun nimrod-ac-completion-prefix ()
  "Starting at current point. Find the point of completion."
  (let ((point (re-search-backward "\\(\\W\\|[\t ]\\)\\([^\\. ]*\\)?"
				   (point-at-bol) t)))
    (if point (1+ point))))

(defun nimrod-ac-completion-candidates (prefix)
  (let ((suggestions (nimrod-call-and-parse-idetools 'suggest)))
    (mapcar (lambda (entry)
              (propertize (nimrod-ide-name entry)
                          'value entry
                          'symbol (assoc-default (nimrod-ide-type entry)
                                                 nimrod-type-abbrevs)
                          'type-sig (nimrod-ide-signature entry)
                          'summary (nimrod-ac-trunc-summary (nimrod-ide-comment entry))
                          ))
            suggestions)))

;;; Copy/pasted from ensime
(defun nimrod-ac-trunc-summary (str)
  (let ((len (length str)))
    (if (> len 40)
	(concat (substring str 0 40) "...")
      str)))

(defun nimrod-call-and-parse-idetools (mode)
  "Call idetools and get `nimrod-ide' structs back."
  (nimrod-parse-idetools-buffer (nimrod-call-idetools mode)))

(defstruct nimrod-ide type namespace name signature path line column comment)

(defun nimrod-parse-idetools-buffer (buffer)
  "Returns a list of `nimrod-ide' structs, based on the contents of `buffer'."
  (with-current-buffer buffer
    (mapcar (lambda (line)
              (destructuring-bind (_ type fn sig path line col comment) (split-string line "\t")
                (make-nimrod-ide
                 :type type
                 :namespace (first (split-string fn "\\."))
                 :name (second (split-string fn "\\."))
                 :signature sig
                 :path path
                 :line (string-to-number line)
                 :column (string-to-number col)
                 :comment comment)))
            (split-string (buffer-string) "[\r\n]" t))))

(defun nimrod-call-idetools (mode)
  "ARGS should be one of `nimrod-idetools-modes'. Grab the data
from the returned buffer."
  (when (not (memq mode nimrod-idetools-modes))
    (error (concat mode " not one from `nimrod-idetools-modes'.")))
  (let ((tempfile (nimrod-save-buffer-temporarly))
        (file (buffer-file-name))
        (buffer (get-buffer-create (format "*nimrod-idetools-%s*" mode))))
    ;; There can only be one. Useful for suggest, not sure about the
    ;; other modes. Change as needed.
    (when (bufferp buffer)
      (with-current-buffer buffer
        (erase-buffer)))
    (let ((args (append (list nimrod-command nil (list buffer (concat temporary-file-directory "nimrod-idetools-stderr")) nil)
                   (remove nil (list
                                "idetools"
                                "--stdout"
                                (nimrod-format-cursor-position file tempfile) ; --trackDirty
                                (when (nimrod-get-project-root)
                                  (format "--include:%s" (nimrod-get-project-root)))
                                (concat "--" (symbol-name mode))
                                ;; in case of no project main file, use the tempfile. Might be
                                ;; useful for repl.
                                (or (nimrod-get-project-main-file) tempfile))))))
      ;; (message (format "%S" args))      ; Debugging
      (apply 'call-process args))
    (delete-directory (file-name-directory tempfile) t)
    buffer))

(defun nimrod-save-buffer-temporarly ()
  "This saves the current buffer and returns the location, so we
  can pass it to idetools."
  (let* ((dirname (make-temp-file "nimrod-suggest" t))
         (filename (concat (file-name-as-directory dirname)
                           (file-name-nondirectory (buffer-file-name)))))
    (save-restriction
      (widen)
      (write-region (point-min) (point-max) filename) nil 'foo)
    filename))

;; From http://stackoverflow.com/questions/14095189/walk-up-the-directory-tree

(defun nimrod-parent-directory (dir)
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))

(defun nimrod-find-file-in-heirarchy (current-dir pattern)
  "Search for a file matching PATTERN upwards through the directory
hierarchy, starting from CURRENT-DIR"
  (let ((parent (nimrod-parent-directory (expand-file-name current-dir))))
    (or (directory-files current-dir t pattern nil)
      (when parent
        (nimrod-find-file-in-heirarchy parent pattern)))))

(defun nimrod-get-project-main-file ()
  "Get the main file for the project."
  (let ((main-file (nimrod-find-file-in-heirarchy
                (file-name-directory (buffer-file-name))
                ".*\.nimrod\.cfg")))
    (when main-file (concat
                     (replace-regexp-in-string "\.nimrod\.cfg$" "" (first main-file))
                     ".nim"))))

(defun nimrod-get-project-root ()
  "Get the project root. Uses `nimrod-get-project-main-file' or git. "
  (or (let ((main-file (nimrod-get-project-main-file)))
        (when main-file (file-name-directory main-file)))
      (let ((git-output (replace-regexp-in-string "\n$" "" 
                                        (with-output-to-string
                                          (with-current-buffer
                                              standard-output
                                            (process-file shell-file-name nil (list t nil) nil shell-command-switch "git rev-parse --show-toplevel"))))))
        (if (< 0 (length git-output))
            git-output
          nil))))

(defun nimrod-format-cursor-position (file tempfile)
  "Formats the position of the cursor to a nice little
--trackDirty statement, referencing the file in the temprorary
directory."
  (format "--trackDirty:%s,%s,%d,%d" tempfile file (line-number-at-pos) (current-column)))

(defun nimrod-goto-sym ()
  "Go to the definition of the symbol currently under the cursor."
  (interactive)
  (let ((def (first (nimrod-call-and-parse-idetools 'def))))
    (when (not def) (error "Symbol not found."))
    (find-file (nimrod-ide-path def))
    (goto-line (nimrod-ide-line def))))

(provide 'nimrod-mode)

(setq auto-mode-alist (cons '("\\.nim$" . nimrod-mode) auto-mode-alist))

;;; nimrod-mode.el ends here
