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
;; -- Make things non-case-sensitive
;; -- Indent to previous # character if we're on a commented line
;; -- Indent to previous start-of-string if line starts with string
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
;; 								             Simple keywords                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Define keywords, etc.
;; ---------------------

(defvar nimrod-keywords
	'("import" "type" "proc" "when" "include" "object" "of" "ptr" "ref"
		"template" "const" "var" "iterator" "else" "while" "elif" "return"
		"break" "for" "in" "yield" "magic" "noSideEffect" "importc" "nodecl" "rtl"
		"enum" )
	"Nimrod keywords.")

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


;; Create regular expressions
;; --------------------------

(defvar nimrod-keywords-regexp (regexp-opt nimrod-keywords 'words))
(defvar nimrod-types-regexp (regexp-opt nimrod-types 'words))
(defvar nimrod-types-regexp (regexp-opt nimrod-exceptions 'words))
(defvar nimrod-constants-regexp (regexp-opt nimrod-constants 'words))
(defvar nimrod-builtins-regexp (regexp-opt nimrod-builtins 'words))
(defvar nimrod-operators-regexp (regexp-opt nimrod-operators 'words))

(defvar nimrod-decimal-regexp
	"\\<[0-9_]+\\(\\.[0-9_]+\\)?\\([eE][0-9]+\\)?\\>"
	"Regular expression for matching decimal literals in Nimrod."
	)

(defvar nimrod-hex-regexp
	"\\<\\0x[0-9a-fA-F_]+\\(\\.[0-9a-fA-F_]+\\)?\\([eE][0-9a-fA-F]+\\)?\\>"
	"Regular expression for matching hexadecimal literals in Nimrod."
	)

(defvar nimrod-octal-regexp
	"\\<\\0o[0-7_]+\\(\\.[0-7_]+\\)?\\([eE][0-7]+\\)?\\>"
	"Regular expression for matching octal literals in Nimrod."
	)

(defvar nimrod-binary-regexp
	"\\<\\0b[01_]+\\(\\.[01_]+\\)?\\([eE][01]+\\)?\\>"
	"Regular expression for matching binary literals in Nimrod."
	)

(defvar nimrod-variables-regexp
	"\\<[a-zA-Z][a-zA-Z0-9_]+\\>"
	"Regular expression for matching variable identifiers in Nimrod."	
	)

(defvar nimrod-tab-regexp "\\(\t+\\)")

;; TODO: make work!
(defface nimrod-tab-face
  '((((class color) (background dark))
     (:background "grey22" :foreground "darkgray"))
    (((class color) (background light))
     (:background "beige"  :foreground "lightgray"))
    (t (:inverse-video t)))
  "Face used to visualize TAB."
  :group 'whitespace)

(setq nimrod-font-lock-keywords
      `(  ;; note the BACKTICK, `
				(,nimrod-tab-regexp . nimrod-tab-face) ;; TODO: make work!
				(,nimrod-keywords-regexp . font-lock-keyword-face)
				(,nimrod-types-regexp . font-lock-type-face)
				(,nimrod-constants-regexp . font-lock-constant-face)
        (,nimrod-builtins-regexp . font-lock-builtin-face)
				(,nimrod-decimal-regexp . font-lock-constant-face)
				(,nimrod-hex-regexp . font-lock-constant-face)
				(,nimrod-octal-regexp . font-lock-constant-face)
				(,nimrod-binary-regexp . font-lock-constant-face)
				(,nimrod-operators-regexp . font-lock-variable-face)
				(,nimrod-variables-regexp . font-lock-variable-face)
				))


(defun nimrod-setup-font-lock ()
	"This will be called when defining nimrod-node, below."
	(setq font-lock-defaults '((nimrod-font-lock-keywords))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 								                Comments                                    ;;
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
;; 								               Indentation                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar nimrod-indent-offset 2 "Number of spaces per level of indentation.")

(defconst nimrod-blank-line-re "^ *$"
  "Regexp matching a line containing only (valid) whitespace.")

(defconst nimrod-new-block-re ".*\\(\=\\|var\\|type\\|const\\|\\:\\)$"
  "Regexp matching a line that precedes a new block.")

(defun nimrod-compute-indentation ()
  "Calculate the maximum sensible indentation for the current line."
  (save-excursion

    (beginning-of-line)
		(forward-line -1)
		(while (and (looking-at nimrod-blank-line-re)
								(> (point) (point-min)))
			(forward-line -1)
			)

		(+ (current-indentation)
			 (if (looking-at nimrod-new-block-re) nimrod-indent-offset 0))))


(defun nimrod-indent-line ()
  "Indent the current line.  The first time this command is used, the line will be indented to the
maximum sensible indentation.  Each immediately subsequent usage will
back-dent the line by `nimrod-indent-offset' spaces.  On reaching column
0, it will cycle back to the maximum sensible indentation."

  (interactive "*")

  (let ((ci (current-indentation))
        (cc (current-column))
        (need (nimrod-compute-indentation)))

    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      (if	(and (equal last-command this-command) (/= ci 0))
					(indent-to (* (/ (- ci 1) nimrod-indent-offset) nimrod-indent-offset))
				  (indent-to need)))

		(if (< (current-column) (current-indentation))
				(forward-to-indentation 0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 								             Wrap it all up ...                             ;;
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

  (modify-syntax-entry ?\' "\""  nimrod-mode-syntax-table)
  (modify-syntax-entry ?\" "\""  nimrod-mode-syntax-table)

	(setq indent-tabs-mode nil) ;; Always indent with SPACES!
	)

(provide 'nimrod-mode)

(setq auto-mode-alist (cons '("\\.nim$" . nimrod-mode) auto-mode-alist))

