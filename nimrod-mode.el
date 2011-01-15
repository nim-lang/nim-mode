
;; Simple keywords
;; ---------------

(defvar nimrod-keywords

	'("import" "type" "proc" "when" "include" "object" "of" "ptr" "ref"
		"template" "const" "var" "iterator" "else" "while" "elif" "return"
		"break" "for" "in" "yield" "magic" "noSideEffect" "importc" "nodecl" "rtl" "enum" )

	"Nimrod keywords.")

(defvar nimrod-types
	'("int" "int8" "int16" "int32" "int64" "float" "float32" "float64"
		"bool" "char" "string" "cstring" "pointer" "Ordinal" "nil" "expr"
		"stmt" "typeDesc" "range" "array" "openarray" "seq" "set"
		"TGenericSeq" "PGenericSeq" "NimStringDesc" "NimString" "Byte"
		"Natural" "Positive" "TObject" "PObject" "TResult" "TEndian"
		"TAddress" "BiggestInt" "BiggestFloat" "cchar" "cschar" "cshort"
		"cint" "clong" "clonglong" "cfloat" "cdouble" "clongdouble"
		"cstringArray" "PFloat32" "PFloat64" "PInt64" "PInt32"
		"TGC_Strategy" "TFile" "TFileMode")
	"Nimrod types defined in <lib/system.nim>."
	)

(defvar nimrod-exceptions
	'("E_Base" "EAsynch" "ESynch" "ESystem" "EIO" "EOS"
		"EInvalidLibrary" "EResourceExhausted" "EArithmetic" "EDivByZero"
		"EOverflow" "EaccessViolation" "EAssertionFailed" "EControlC"
		"EInvalidValue" "EOutOfMemory" "EInvalidIndex" "EInvalidField"
		"EOutOfRange" "EStackOverflow" "ENoExceptionToReraise"
		"EInvalidObjectAssignment" "EInvalidObjectConversion"
		"EFloatingPoint" "EFloatingInvalidOp" "EFloatDivByZero"
		"EFloatOverflow" "EFloatUnderflow" "EFloatInexact")
	"Nimrod exceptions defined in <lib/system.nim>.")

(defvar nimrod-constants
	'("isMainModule" "CompileDate" "CompileTime" "NimrodVersion"
		"NimrodMajor" "NimrodMinor" "NimrodPatch" "cpuEndian" "hostOS"
		"hostCPU" "appType" "inf" "neginf" "nan" "QuitSuccess"
		"QuitFailure" "stdin" "stdout" "stderr" "true" "false" )
	"Nimrod constants defined in <lib/system.nim>.")

(defvar nimrod-builtins 

	'("defined" "definedInScope" "not" "+" "-" "=" "<" ">" "@" "&" "*"
		">=" "<=" "$" ">=%" ">%" "<%" "<=%" "," ":" "==" "/"  "div" "mod"
		"shr" "shl" "and" "or" "xor" "abs" "+%" "-%" "*%" "/%" "%%" "-+-"
		"not_in" "is_not" "cmp" "high" "low" "sizeof" "succ" "pred" "inc"
		"dec" "newSeq" "len" "incl" "excl" "card" "ord" "chr" "ze" "ze64"
		"toU8" "toU16" "toU32" "min" "max" "setLen" "newString" "add"
		"compileOption" "del" "delete" "insert" "repr" "toFloat"
		"toBiggestFloat" "toInt" "toBiggestInt" "addQuitProc" "copy"
		"zeroMem" "copyMem" "moveMem" "equalMem" "alloc" "alloc0"
		"realloc" "dealloc" "assert" "swap" "getRefcount" "getOccupiedMem"
		"getFreeMem" "getTotalMem" "countdown" "countup" "items"
		"enumerate" "isNil" "find" "contains" "pop" "each" "GC_disable"
		"GC_enable" "GC_fullCollect" "GC_setStrategy"
		"GC_enableMarkAndSweep" "GC_disableMarkAndSweep"
		"GC_getStatistics" "GC_ref" "GC_unref" "accumulateResult" "echo"
		"newException" "quit" "open" "reopen" "close" "endoffile"
		"readchar" "flushfile" "readfile" "write" "readline" "writeln"
		"getfilesize" "readbytes" "readchars" "readbuffer" "writebytes"
		"writechars" "writebuffer" "setfilepos" "getfilepos" "lines"
		"filehandle" "cstringArrayToSeq" "getDiscriminant" "selectBranch"
		"getCurrentException" "getCurrentExceptionMsg" "likely" "unlikely"
		)

	"Standard library functions fundamental enough to count as builtins. Magic functions."
)

(defvar nimrod-operators
	'( "`" "{." ".}" "[" "]" "{" "}" "(" ")" )
	"Nimrod standard operators.")

(defvar nimrod-keywords-regexp (regexp-opt nimrod-keywords 'words))
(defvar nimrod-types-regexp (regexp-opt (append nimrod-types nimrod-exceptions) 'words))
(defvar nimrod-constants-regexp (regexp-opt nimrod-constants 'words))
(defvar nimrod-builtins-regexp (regexp-opt nimrod-builtins 'words))
(defvar nimrod-operators-regexp (regexp-opt nimrod-operators 'words))

(defvar nimrod-decimal-regexp "\\<[0-9_]+\\(\\.[0-9_]+\\)?\\([eE][0-9]+\\)?\\>")
(defvar nimrod-hex-regexp "\\<\\0x[0-9a-fA-F_]+\\(\\.[0-9a-fA-F_]+\\)?\\([eE][0-9a-fA-F]+\\)?\\>")
(defvar nimrod-octal-regexp "\\<\\0o[0-7_]+\\(\\.[0-7_]+\\)?\\([eE][0-7]+\\)?\\>")
(defvar nimrod-binary-regexp "\\<\\0b[01_]+\\(\\.[01_]+\\)?\\([eE][01]+\\)?\\>")
(defvar nimrod-variables-regexp "\\<[a-zA-Z][a-zA-Z0-9_]+\\>")

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


;; Comments
;; --------

;; the command to comment/uncomment text
(defun nimrod-comment-dwim (arg)
	"Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
	(interactive "*P")
	(require 'newcomment)
	(let ((deactivate-mark nil) (comment-start "#") (comment-end ""))
		(comment-dwim arg)))


;; Indentation
;; -----------

(defvar nimrod-indent-offset 2
  "Number of spaces per level of indentation.")


(defconst nimrod-blank-line-re "^ *$"
  "Regexp matching a line containing only (valid) whitespace.")

(defconst nimrod-new-block-re ".*\\(\=\\|var\\|type\\|const\\|\\:\\)$"
  "Regexp matching a line containing only (valid) whitespace.")

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


;; Wrap it all up
;; --------------

(define-derived-mode nimrod-mode fundamental-mode
	"nimrod mode"
	"Major mode for the Nimrod programming language"

	(setq mode-name "Nimrod")

	(setq font-lock-defaults '((nimrod-font-lock-keywords)))


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

;; TODO
;; ----

;; Make things non-case-sensitive

;; Indent to previous # character if we're on a commented line

;; Identifier following "proc" gets font-lock-function-name-face

;; Treat proc ___ ( ... ) parameter list separately

;; Make double-# comments get font-lock-doc-face.

;; Highlight tabs as syntax error.

;; Treat things inside {. ... .} separately.
