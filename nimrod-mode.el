;;; nimrod-mode.el --- A major mode for the Nimrod programming language
;; 
;; Filename: nimrod-mode.el
;; Description: A major mode for the Nimrod programming language
;; Author: James H. Fisher <jameshfisher@gmail.com>
;; Maintainer: James H. Fisher <jameshfisher@gmail.com>
;; Created: Sun Jan 16 2011
;; Version: 0.0.1
;; Last-Updated: Sun Jan 16 2011
;;           By: James H. Fisher <jameshfisher@gmail.com>
;;     Update #: 1
;; Keywords: nimrod
;; Compatibility: GNU Emacs 23
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Installation: Place this file on your load path, and put this in
;; your init file (`.emacs'):
;;
;; (require 'nimrod-mode)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;;
;; 2011/01/16
;;     Initial release of nimrod-mode.el.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (split-string "addr and as asm atomic
bind block break
case cast const continue converter
discard distinct div
elif else end enum except
finally for from generic
if implies import in include is isnot iterator
lambda let
macro method mod
nil not notin
object of or out
proc ptr
raise ref return
shl shr
template try tuple type
var
when while with without
xor
yield")
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

(defvar nimrod-triple-quote-string-regexp
  "\\<\"\"\".*\"\"\"\\>"
  "Regular expression for matching triple quote strings."
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
        (,nimrod-character-literal-regexp . font-lock-constant-face)
        (,nimrod-raw-string-regexp . font-lock-string-face)
        (,nimrod-triple-quote-string-regexp . font-lock-string-face)
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

(define-derived-mode nimrod-mode fundamental-mode
  "nimrod mode"
  "A major mode for the Nimrod programming language."

  (setq mode-name "Nimrod")  ;; This will display in the mode line.

  (nimrod-setup-font-lock)



  ;; modify the keymap
  (define-key nimrod-mode-map [remap comment-dwim] 'nimrod-comment-dwim)

  (set (make-local-variable 'indent-line-function) 'nimrod-indent-line)

  ;; Documentation comment highlighting
  ;; (modify-syntax-entry ?\# ". 12b" nimrod-mode-syntax-table)
  ;; (modify-syntax-entry ?\n "> b" nimrod-mode-syntax-table)

  ;; Comment highlighting
  (modify-syntax-entry ?# "< b"  nimrod-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" nimrod-mode-syntax-table)

  (modify-syntax-entry ?\' "w"  nimrod-mode-syntax-table)
  (modify-syntax-entry ?\" "w"  nimrod-mode-syntax-table)

  (setq indent-tabs-mode nil) ;; Always indent with SPACES!
  )

(provide 'nimrod-mode)

(setq auto-mode-alist (cons '("\\.nim$" . nimrod-mode) auto-mode-alist))

