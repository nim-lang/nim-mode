;;; nim-vars.el --- nim-mode's variables -*- lexical-binding: t -*-

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(defgroup nim nil
  "A major mode for the Nim programming language."
  :link '(url-link "http://nim-lang.org/")
  :group 'languages)

(defcustom nim-type-abbrevs '(("skProc"         . "f")
                              ("skIterator"     . "i")
                              ("skTemplate"     . "T")
                              ("skType"         . "t")
                              ("skMethod"       . "f")
                              ("skEnumField"    . "e")
                              ("skGenericParam" . "p")
                              ("skParam"        . "p")
                              ("skModule"       . "m")
                              ("skConverter"    . "C")
                              ("skMacro"        . "M")
                              ("skField"        . "F")
                              ("skForVar"       . "v")
                              ("skVar"          . "v")
                              ("skLet"          . "v")
                              ("skLabel"        . "l")
                              ("skConst"        . "c")
                              ("skResult"       . "r"))
  "Abbrevs for nim-mode (used by company)."
  :type 'assoc
  :group 'nim)

(defface nim-tab-face
  '((((class color) (background dark))
     (:background "grey22" :foreground "darkgray"))
    (((class color) (background light))
     (:background "beige"  :foreground "lightgray"))
    (t (:inverse-video t)))
  "Face used to visualize TAB."
  :group 'nim)

(defface nim-font-lock-export-face
  '((t :weight bold
       :slant italic
       :inherit font-lock-function-name-face))
  "Font Lock face for export (XXX*)"
  :group 'nim)

(defface nim-font-lock-pragma-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Font Lock face for pragmas."
  :group 'nim)

(defface nim-non-overloadable-face
  '((t :inherit font-lock-builtin-face
       :slant italic))
  "Font Lock face for nonoverloadable builtins."
  :group 'nim)

(defface nim-font-lock-number-face
  '((t :slant italic))
  "Font Lock face for numbers."
  :group 'nim)

(defcustom nim-indent-trigger-commands
  '(indent-for-tab-command yas-expand yas/expand)
  "Commands that might trigger a `nim-indent-line' call."
  :type '(repeat symbol)
  :group 'nim)

(defcustom nim-indent-offset 2
  "Number of spaces per level of indentation."
  :type 'integer
  :group 'nim)

(defcustom nim-smie-function-indent 4
  "Number of spaces between ‘proc ... =’.
Note that this configuration affects other ‘template’, ‘macro’,
‘iterator’, and ‘converter’ declaration as well."
  :type 'integer
  :group 'nim)

(defcustom nim-smie-indent-stoppers
  '("proc" "template" "macro" "iterator" "converter" "type")
  "Indentation behavior after empty line.
You can specify list of string, which you want to stop indenting.
If it’s nil, it does nothing."
  :type '(choice
          (repeat :tag "" string)
          (const :tag "" nil))
  :group 'nim)

(defcustom nim-smie-indent-dedenters 'all-dedent
  "Indentation behavior after empty line.
If you set ‘all-dedent’, it forces dedent whatever point starts.
Or you can specify list of string, which you want to dedent.
If it’s nil, it does nothing."
  :type '(choice
          (repeat :tag "Dedenter symbols" string)
          (const :tag "Don't dedent" nil)
          (const :tag
                 "Dedent all if previous line is empty line" all-dedent))
  :group 'nim)

(defcustom nim-smie-dedent-after-break '()
  "List of string that dedent after break statement.
This feature is activated if only the break line has
other tokens like ’:’ or ’=’."
  :type '(choise
          (repeat :tag "List of dedenter token" string)
          (const :tag "" nil)))

(defcustom nim-smie-after-indent-hook nil
  "Hook run after indenting."
  :type 'hook
  :group 'nim)

(defcustom nim-mode-init-hook nil
  "This hook is called when ‘nim-mode’ is initialized."
  :type 'hook
  :group 'nim)

(defcustom nim-suggest-options '("--v2")
  "Options for Nimsuggest.
Note that ‘--verbosity:0’ and ‘--epc’ are automatically passed nim-mode’s
epc function."
  :type '(choice (repeat :tag "List of options" string)
                 (const :tag "" nil))
  :group 'nim)

(defvar nim-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.") 'nim-goto-sym)
    (define-key map (kbd "C-c h") 'nim-explain-sym)
    (define-key map ":" 'nim-indent-electric-colon)
    (define-key map "\C-c<" 'nim-indent-shift-left)
    (define-key map "\C-c>" 'nim-indent-shift-right)
    map))

(defconst nim-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Give punctuation syntax to ASCII that normally has symbol
    ;; syntax or has word syntax and isn't a letter.
    (let ((symbol (string-to-syntax "_"))
          (sst (standard-syntax-table)))
      (dotimes (i 128)
        (unless (= i ?_)
          (if (equal symbol (aref sst i))
              (modify-syntax-entry i "." table)))))
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?% "." table)

    ;; Comment
    (modify-syntax-entry ?# "." table)
    (modify-syntax-entry ?\n ">" table)
    ;; Use "." Punctuation syntax class because I got error when I
    ;; used "$" from smie.el
    (modify-syntax-entry ?` "'" table)

    ;; Use _ syntax to single quote
    ;; See also `nim-syntax-propertize-function'.
    (modify-syntax-entry ?\' "_" table)

    ;; Parentheses
    (modify-syntax-entry ?\[ "(]  " table)
    (modify-syntax-entry ?\] ")[  " table)
    (modify-syntax-entry ?\{ "(}  " table)
    (modify-syntax-entry ?\} "){  " table)
    (modify-syntax-entry ?\( "()  " table)
    (modify-syntax-entry ?\) ")(  " table)
    table)
  "Syntax table for Nim files.")

(defvar nim-dotty-syntax-table
  (let ((table (make-syntax-table nim-mode-syntax-table)))
    (modify-syntax-entry ?. "w" table)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Dotty syntax table for Nim files.
It makes underscores and dots word constituent chars.")

(defconst nim-comment
  `((single
     . ((comment-start      . "#")
        (comment-end        . "")
        (comment-start-skip . ,(rx "#" (? "#") (? " ")))
        (comment-use-syntax . t)))
    (multi
     . ((comment-start      . "#[")
        (comment-end        . "]#")
        (comment-start-skip
         . ,(rx (group
                 (syntax comment-start) (? "#") "[")))
        (comment-end-skip
         . ,(rx (group
                 "]#" (? "#"))))
        ;; comment-continue has to include non space character
        ;; otherwise it makes trouble when you do ‘uncomment-region’.
        (comment-continue   . " |")
        (comment-padding    . "  ")
        (comment-multi-line . t)
        (comment-use-syntax . nil)))))

(defconst nim-keywords
  '("addr" "and" "as" "asm" "atomic" "bind" "block" "break" "case"
    "cast" "const" "continue" "converter" "discard" "distinct" "div" "do"
    "elif" "else" "end" "enum" "except" "export" "finally" "for" "from"
    "generic" "if" "import" "in" "include" "interface" "isnot"
    "iterator" "lambda" "let" "macro" "method" "mixin" "mod" "nil" "not"
    "notin" "object" "of" "or" "out" "proc" "ptr" "raise" "ref" "return"
    "shared" "shl" "shr" "static" "template" "try" "tuple" "type" "var"
    "when" "while" "with" "without" "xor" "yield")
  "Nim keywords.
The above string is taken from URL
`http://nim-lang.org/manual.html#identifiers-keywords', for easy
updating.")

(defconst nim-types
  '("int" "int8" "int16" "int32" "int64" "uint" "uint8" "uint16" "uint32"
    "uint64" "float" "float32" "float64" "bool" "char" "string" "cstring"
    "pointer" "expr" "stmt" "typedesc" "void" "auto" "any"
    "untyped" "typed" "range" "array" "openArray" "Ordinal" "seq" "set"
    "TGenericSeq" "PGenericSeq" "NimStringDesc" "NimString" "byte" "Natural"
    "Positive" "RootObj" "RootRef" "RootEffect" "TimeEffect" "IOEffect"
    "ReadIOEffect" "WriteIOEffect" "ExecIOEffect"
    "TResult" "Endianness" "ByteAddress" "BiggestInt" "BiggestFloat"
    "cchar" "cschar" "cshort" "cint" "clong" "clonglong" "cfloat" "cdouble"
    "clongdouble" "cstringArray" "PFloat32" "PFloat64" "PInt64" "PInt32"
    "SomeSignedInt" "SomeUnsignedInt" "SomeInteger" "SomeOrdinal" "SomeReal"
    "SomeNumber" "Slice" "shared" "guarded"
    "NimNode" "GC_Strategy" "File" "FileHandle" "FileMode"
    "TaintedString" "PFrame" "TFrame")
  "Nim types defined in <lib/system.nim>.")

(defconst nim-exceptions
  '("Exception" "SystemError" "IOError" "OSError" "LibraryError"
    "ResourceExhaustedError" "ArithmeticError" "DivByZeroError" "OverflowError"
    "AccessViolationError" "AssertionError" "ValueError" "KeyError"
    "OutOfMemError" "IndexError" "FieldError" "RangeError"
    "StackOverflowError" "ReraiseError" "ObjectAssignmentError"
    "ObjectConversionError" "DeadThreadError" "FloatInexactError"
    "FloatUnderflowError" "FloatingPointError" "FloatInvalidOpError"
    "FloatDivByZeroError" "FloatOverflowError")
  "Nim exceptions defined in <lib/system.nim>.")

(defconst nim-constants
  '("isMainModule" "CompileDate" "CompileTime" "NimVersion"
    "NimMajor" "NimMinor" "NimPatch" "NimStackTrace" "cpuEndian" "hostOS"
    "hostCPU" "appType" "Inf" "NegInf" "NaN" "nimvm" "QuitSuccess"
    "QuitFailure" "stdin" "stdout" "stderr" "true" "false"
    "on" "off" "NoFakeVars")
  "Nim constants defined in <lib/system.nim>.")

(defconst nim-nonoverloadable-builtins
  '("declared" "defined" "definedInScope" "compiles" "low" "high" "sizeOf"
    "is" "of" "shallowCopy" "getAst" "astToStr" "spawn" "procCall")
  "Nim nonoverloadable builtins.")

(defconst nim-builtins
  '("not" "+" "-" "=" "<" ">" "@" "&" "*"
    ">=" "<=" "$" ">=%" ">%" "<%" "<=%" "," ":" "==" "/"  "div" "mod"
    "shr" "shl" "and" "or" "xor" "abs" "+%" "-%" "*%" "/%" "%%" "-+-"
    "not_in" "is_not" "cmp" "succ" "pred" "inc"
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
Magic functions.")

(defconst nim-operators
  '( "`" "{." ".}" "[" "]" "{" "}" "(" ")" )
  "Nim standard operators.")

;; Nimscript
(defvar nim-nimble-ini-format-regex (rx line-start "[Package]"))

(defconst nimscript-keywords
  `((,(rx (group symbol-start "task" symbol-end) (1+ " ")
          (? (group  symbol-start (or "build" "tests" "bench") symbol-end)))
     (1 font-lock-keyword-face)
     (2 font-lock-builtin-face nil t))
    (,(rx (group symbol-start
                 ;; Just picked up only no upper case functions.
                 (or "cd" "exec" "cmpic" "put" "get" "exists" "log"
                     "strip" "switch" "requires")
                 symbol-end))
     (0 font-lock-keyword-face))
    ("\\_<ScriptMode\\_>"
     (0 font-lock-type-face))
    (,(rx symbol-start
          (or "packageName" "version" "author" "description" "license"
              "srcDir" "binDir" "backend" "mode")
          symbol-end)
     (0 font-lock-variable-name-face))))

(provide 'nim-vars)
;;; nim-vars.el ends here
