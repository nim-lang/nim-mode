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

(defcustom nim-common-init-hook nil
  "A hook for both nim-mode and nimscript-mode."
  :type 'hook
  :group 'nim)

(defcustom nim-capf-after-exit-function-hook nil
  "A hook that is called with an argument.
The argument is string that has some properties."
  :type 'hook
  :group 'nim)

(defcustom nim-pretty-triple-double-quotes
  ;; What character should be default? („…“, “…”, ‘…’, or etc.?)
  (cons ?„ ?”)
  "Change triple double quotes to another quote form.
This configuration is enabled only in `prettify-symbols-mode`."
  :type 'cons
  :group 'nim)

(defcustom nim-compile-command "nim"
  "Path to the nim executable.
You don't need to set this if the nim executable is inside your PATH."
  :type 'string
  :group 'nim)

(defcustom nim-compile-user-args '()
  "The arguments to pass to `nim-compile-command' to compile a file."
  :type '(repeat string)
  :group 'nim)

(defcustom nim-nimsuggest-path (executable-find "nimsuggest")
  "Path to the nimsuggest binary."
  :type '(choice (const :tag "Path of nimsuggest binary" string)
                 (const :tag "" nil))
  :group 'nim)

(defcustom nim-suggest-options '("--v2")
  "Options for Nimsuggest.
Note that ‘--verbosity:0’ and ‘--epc’ are automatically passed nim-mode’s
epc function."
  :type '(choice (repeat :tag "List of options" string)
                 (const :tag "" nil))
  :group 'nim)

(defvar nim-suggest-local-options '()
  "Options for Nimsuggest.
Please use this variable to set nimsuggest’s options for
specific directory or buffer.  See also ‘dir-locals-file’.")

(defvar nim-suggest-ignore-dir-regex
  (rx (or "\\" "/") (in "nN") "im" (or "\\" "/") "compiler" (or "\\" "/")))
(defvar nim-inside-compiler-dir-p nil)

(defvar nim-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c h") 'nim-explain-sym)
    (define-key map (kbd "C-c C-c") 'nim-compile)
    (define-key map "\C-c<" 'nim-indent-shift-left)
    (define-key map "\C-c>" 'nim-indent-shift-right)
    map))

;; Turn off syntax highlight for big files
;; FIXME: what number should we set as default?
(defcustom nim-syntax-disable-limit 400000
  "Number of buffer size limit to turn off some syntax highlights.
See also ‘nim-syntax-disable-keywords-list’."
  :type '(choice (const :tag "unsigned integer" number)
                 (const :tag "nil" nil))
  :group 'nim)

;; Syntax table
(defvar nim-mode-syntax-table
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

;;;;;;;;;;;;;;;;;;;;
;; Nim keywords

;; Those keywords are used to syntax highlight as well as
;; auto-completion. If you want to change those keywords,
;; please consider about auto-completion; it inserts what
;; you registered. (snark or camel)

(defconst nim-keywords
  '("addr" "and" "as" "asm" "atomic" "bind" "block" "break" "case"
    "cast" "concept" "const" "continue" "converter" "defer" "discard" "distinct"
    "div" "do" "elif" "else" "end" "enum" "except" "export" "finally" "for"
    "from" "generic" "if" "import" "in" "include" "interface" "isnot"
    "iterator" "lambda" "let" "macro" "method" "mixin" "mod" "nil" "not"
    "notin" "object" "of" "or" "out" "proc" "ptr" "raise" "ref" "return"
    "shared" "shl" "shr" "static" "template" "try" "tuple" "type" "using"
    "var" "when" "while" "with" "without" "xor" "yield")
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

(defconst nim-variables
  '("programResult" "globalRaiseHook" "localRaiseHook" "outOfMemHook"
    ;; not nimscript
    "stdin" "stdout" "stderr")
  "Nim variables defined in <lib/system.nim>.")

(defconst nim-constants
  '("isMainModule" "CompileDate" "CompileTime" "NimVersion"
    "NimMajor" "NimMinor" "NimPatch" "NimStackTrace" "cpuEndian" "hostOS"
    "hostCPU" "appType" "Inf" "NegInf" "NaN" "nimvm" "QuitSuccess"
    "QuitFailure" "true" "false" "nil"
    "on" "off" "NoFakeVars")
  "Nim constants defined in <lib/system.nim>.")

(defconst nim-nonoverloadable-builtins
  '("declared" "defined" "definedInScope" "compiles" "low" "high" "sizeOf"
    "is" "of" "shallowCopy" "getAst" "astToStr" "spawn" "procCall")
  "Nim nonoverloadable builtins.")

(defconst nim-builtins
  '(; length 1 characters are ignored to highlight
    "+" "-" "=" "<" ">" "@" "&" "*" "/"
    ">=" "<=" "$" ">=%" ">%" "<%" "<=%" "==" "+%" "-%" "*%" "/%" "%%"
    "div" "mod" "shr" "shl" "and" "or" "xor"
    "not" "notin" "isnot" "cmp" "succ" "pred" "inc"
    "dec" "newseq" "len" "xlen" "incl" "excl" "card" "ord" "chr" "ze" "ze64"
    "toU8" "toU16" "toU32" "min" "max" "setLen" "newString" "add"
    "compileOption" "del" "delete" "insert" "repr" "toFloat"
    "toBiggestFloat" "toInt" "toBiggestInt" "addQuitProc" "copy"
    "slurp" "staticRead" "gorge" "staticExec" "instantiationInfo"
    "currentSourcePath" "raiseAssert" "failedAssertImpl" "assert" "doAssert"
    "onFailedAssert" "shallow" "eval" "locals"
    "swap" "getRefcount" "countdown" "countup" "min" "max" "abs" "clamp"
    "items" "mitems" "pairs" "mpairs" "isNil"
    "find" "contains" "pop" "fields" "fieldPairs" "each"
    "accumulateresult" "echo" "debugEcho" "newException"
    "getTypeInfo" "quit" "open" "reopen" "close" "endOfFile"
    "readChar" "flushFile" "readAll" "readFile" "write" "writeFile"
    "readLine" "writeLn" "writeLine"
    "getFileSize" "readBytes" "readChars" "readBuffer" "writeBytes"
    "writeChars" "writeBuffer" "setFilePos" "getFilePos" "getFileHandle"
    "lines" "cstringArrayToSeq" "getDiscriminant" "selectBranch"
    ;; hasAlloc
    "safeAdd")
  "Standard library functions fundamental enough to count as builtins.
Magic functions.")

(defconst nim-builtins-without-nimscript
  '(;; hasAlloc && not nimscript && not JS
    "deepCopy"
    ;; not nimscirpt
    "zeroMem" "copyMem" "moveMem" "equalMem"
    ;; not nimscirpt && hasAlloc
    "alloc" "createU" "alloc0" "create" "realloc" "resize" "dealloc"
    "allocShared" "createShareU" "allocShared0" "createShared"
    "reallocShared" "resizeShared" "deallocShared" "freeShared"
    "getOccupiedMem" "getFreeMem" "getTotalMem"
    "GC_disable" "GC_enable" "GC_fullCollect" "GC_setStrategy"
    "GC_enableMarkAndSweep" "GC_disableMarkAndSweep"
    "GC_getStatistics" "GC_ref" "GC_unref"
    ;; not nimscirpt && hasAlloc && hasThreadSupport
    "getOccupiedSharedMem" "getFreeSharedMem" "getTotalSharedMem"
    ;; not nimscirpt && Not JS
    "likely" "unlikely" "rawProc" "rawEnv" "finished"
    ;; not nimscirpt && not hostOS "standalone" && Not JS
    "getCurrentException" "getCurrentExceptionMsg" "onRaise"
    "setCurrentException")
  "Builtin functions copied from system.nim.
But all those functions can not use in NimScript.")

(defconst nim-operators
  '( "`" "{." ".}" "[" "]" "{" "}" "(" ")" )
  "Nim standard operators.")

;; Nimscript
(defvar nim-nimble-ini-format-regex (rx line-start "[Package]"))

(defconst nimscript-builtins
  '("listDirs" "listFiles" "paramStr" "paramCount" "switch" "getCommand"
    "setCommand" "cmpic" "getEnv" "existsEnv" "fileExists" "dirExists"
    "existsFile" "existsDir" "toExe" "toDll" "rmDir" "rmFile" "mkDir"
    "mvFile" "cpFile" "exec" "put" "get" "exists" "nimcacheDir" "thisDir"
    "cd" "requires"
    ;; templates
    "--" "withDir" "task"))

(defconst nimscript-variables
  '("packageName" "version" "author" "description" "license"
    "srcDir" "binDir" "backend" "mode" "skipDirs" "skipFiles"
    "skipExt" "installDirs" "installFiles" "installExt" "bin"
    "requiresData"))

(defvar nimsuggest-check-vervosity "--verbosity:1"
  "Verbosity for chk option.  Current no meaning though.")

(defconst nim-pragmas
  ;; alist of (PRAGMA_NAME . DESCRIPTION)
  ;; the DESCRIPTION can be either a string or list of string.
  ;; Use list of string when you want to describe a pragma, but there
  ;; are more than two ways to use. I.e, pure pragma
  ;; (though I don't implemented displaying list of string yet)
  '(("deprecated" .
     ("deprecate a symbol"
      "[OLD: NEW, ...] -- deprecate symbols"))
    ("noSideEffect" .
     "used to mark a proc/iterator to have no side effects")
    ("procvar" .
     "used to mark a proc that it can be passed to a procedural variable.")
    ("destructor" .
     "used to mark a proc to act as a type destructor. [duplicated]")
    ("compileTime" .
     "used to mark a proc or variable to be used at compile time only")
    ("noReturn" .
     "The ``noreturn`` pragma is used to mark a proc that never returns")
    ("acyclic" .
     "can be used for object types to mark them as acyclic")
    ("final" .
     "can be used for an object type to specify that it cannot be inherited from.")
    ("pure" .
     ("object type can be marked to its type field, which is used for runtime type identification is omitted"
      "enum type can be marked to access of its fields always requires full qualification"))
    ("shallow" .
     "affects the semantics of a type: The compiler is allowed to make a shallow copy.")
    ("asmNoStackFrame" .
     "A proc can be marked with this pragma to tell the compiler it should not generate a stack frame for the proc.")
    ("error" .
     ("used to make the compiler output an error message with the given content."
      "can be used to annotate a symbol (like an iterator or proc)"))
    ("fatal" .
     "In contrast to the error pragma, compilation is guaranteed to be aborted by this pragma.")
    ("warning" .
     "is used to make the compiler output a warning message with the given content. Compilation continues after the warning.")
    ("hint" .
     "is used to make/disable the compiler output a hint message with the given content.")
    ("line" .
     "can be used to affect line information of the annotated statement as seen in stack backtraces")
    ("linearScanEnd" .
     "can be used to tell the compiler how to compile a Nim `case`:idx: statement. Syntactically it has to be used as a statement")
    ("computedGoto".
     "can be used to tell the compiler how to compile a Nim `case`:idx: in a ``while true`` statement.")
    ("unroll" .
     "can be used to tell the compiler that it should unroll a `for`:idx: or `while`:idx: loop for runtime efficiency")
    ("register".
     "declares variable as register, giving the compiler a hint that the variable should be placed in a hardware register for faster access.")
    ("global" .
     "can be applied to a variable within a proc to instruct the compiler to store it in a global location and initialize it once at program startup.")
    ("deadCodeElim" .
     "on -- tells the compiler to activate (or deactivate) dead code elimination for the module the pragma appears in.")
    ("noForward" .
     "on|off -- no forward declaration")
    ("pragma" .
     "can be used to declare user defined pragmas.")
    ("experimental" .
     "enables experimental language features.")
    ("push" .
     "are used to override the settings temporarily with pop")
    ("pop" .
     "are used to override the settings temporarily with push")
    ;; implementation specific pragmas
    ("bitsize" .
     "is for object field members. It declares the field as a bitfield in C/C++.")
    ("volatile" .
     "is for variables only. It declares the variable as `volatile`, whatever that means in C/C++.")
    ("noDecl" .
     "tell Nim that it should not generate a declaration for the symbol in the C code.")
    ("header" .
     "can be applied to almost any symbol and specifies that it should not be declared and instead the generated code should contain an `#include`")
    ("incompleteStruct" .
     "tells the compiler to not use the underlying C ``struct`` in a ``sizeof`` expression")
    ("compile" .
     "STRING -- can be used to compile and link a C/C++ source file with the project")
    ("link" .
     "STRING -- can be used to link an additional file with the project")
    ("passC" .
     ("STRING -- can be used to pass additional parameters to the C compiler like commandline switch `--passC`"
      "you can use `gorge` from the `system module` to embed parameters from an external command at compile time"))
    ("passL" .
     ("STRING -- can be used to pass additional parameters to the linker like commandline switch `--passL`"
      "you can use `gorge` from the `system module` to embed parameters from an external command at compile time"))
    ("emit" .
     "STRING -- can be used to directly affect the output of the compiler's code generator.")
    ("importcpp" . "")
    ("importobjc" . "")
    ("codegenDecl" .
     "STRING -- can be used to directly influence Nim's code generator.")
    ("injectStmt" .
     "can be used to inject a statement before every other statement in the current module.")
    ("intdefine" . "Reads in a build-time define as an integer")
    ("strdefine" . "Reads in a build-time define as a string")
    ;; Compilation option pragmas
    ("checks" .
     "on|off -- Turns the code generation for all runtime checks on or off.")
    ("boundChecks" .
     "on|off -- Turns the code generation for array bound checks on or off.")
    ("overflowChecks" .
     "on|off -- Turns the code generation for over- or underflow checks on or off.")
    ("nilChecks" .
     "on|off -- Turns the code generation for nil pointer checks on or off.")
    ("assertions" .
     "on|off -- Turns the code generation for assertions on or off.")
    ("warnings" .
     "on|off -- Turns the warning messages of the compiler on or off.")
    ("hints" .
     "on|off -- Turns the hint messages of the compiler on or off.")
    ("optimization" .
     "none|speed|size -- optimize the code for the options")
    ("callconv" .
     "cdecl|... -- Specifies the default calling convention for all procedures (and procedure types) that follow.")
    ;; ffi.txt
    ("importc" . "STRING")
    ("exportc" . "STRING")
    ("extern" . "STRING")
    ("bycopy" . "can be applied to an object or tuple type and instructs the compiler to pass the type by value to procs")
    ("byref" . "can be applied to an object or tuple type and instructs the compiler to pass the type by reference (hidden pointer) to procs.")
    ("varargs" . "tells Nim that the proc can take a variable number of parameters after the last specified parameter.")
    ("union" . "can be applied to any ``object`` type. It means all of the object's fields are overlaid in memory.")
    ("packed" . "ensures that the fields of an object are packed back-to-back in memory.")
    ("unchecked" . "can be used to mark a named array as `unchecked` meaning its bounds are not checked.")
    ("dynlib" . "STRING -- dynamic library")
    ;; threads.txt
    ("thread" . "")
    ("threadvar" . "")
    ;; locking.txt
    ("guard" . "")
    ("locks" . "[X, ...]")
    ;; effects.txt
    ("raises" . "[EXCEPTION, ...] -- can be used to explicitly define which exceptions a proc/iterator/method/converter is allowed to raise.")
    ("tags" . "[TYPE, ...]")
    ("effects" . "")
    ;; types.txt
    ("nimcall" .
     "default convention used for a Nim proc. It is the same as `fastcall`, but only for C compilers that support `fastcall`")
    ("closure" .
     "default calling convention for a procedural type that lacks any pragma annotations.")
    ("stdcall" .
     "convention as specified by Microsoft; the generated C procedure is declared with `__stdcall` keyword.")
    ("cdecl" .
     "The cdecl convention means that a procedure shall use the same convention as the C compiler.")
    ("safecall" . "")
    ("inline" . "")
    ("fastcall" . "means different things to different C compilers. One gets whatever the C `__fastcall` means.")
    ("syscall" . "syscall convention is the same as `__syscall` in C. It is used for interrupts.")
    ("noconv" . "generated C code will not have any explicit calling convention and thus use the C compiler's default calling convention.")
    ("inheritable" . "introduce new object roots apart from `system.RootObj`")
    ;; http://forum.nim-lang.org/t/1100
    ;; copied Araq's explanation
    ("gensym" . "generate a fresh temporary variable here for every instantiation to resemble function call semantics")
    ("dirty" . "everything is resolved in the instantiation context")
    ("inject" . "the instantiation scope sees this symbol")
    ("immediate" . "don't resolve types and expand this thing eagerly")
    ;; http://nim-lang.org/docs/manual.html#statements-and-expressions-var-statement
    ("noInit" . "avoid implicit initialization for `var`")
    ("requiresInit" . "avoid implicit initialization for `var` and it does a control flow analysis to prove the variable has been initialized and does not rely on syntactic properties")
    ;; http://nim-lang.org/docs/manual.html#types-pre-defined-floating-point-types
    ("NanChecks" . "on|off")
    ("InfChecks" . "on|off")
    ("floatChecks" "on|off")
    ;;; not sure where to look
    ("noinline" . "")
    ("benign" . "")
    ("profiler" . "")
    ("stackTrace" . "")
    ("sideEffect" . "")
    ("compilerRtl" . "")
    ("merge" . "")
    ("gcsafe" . "")
    ("rtl" . "")
    ;; from 14.0
    ("this" . "self|ID -- automatic self insertion")
    ;; seems related to this: http://nim-lang.org/docs/intern.html#how-the-rtl-is-compiled
    ;; but not sure...
    ("compilerProc" . "")
    ("magic" . "")
    )
  "Alist of (pragma name . description).
The description is unofficial; PRs are welcome.")

;; obsolete
(defvar nimsuggest-vervosity "--verbosity:0"
  "This variable will not be needed for latest nimsuggest.
Please set this value to nil if you have latest nimsuggest,
which supports ‘chk’ option for EPC.")

(make-obsolete-variable
 'nimsuggest-vervosity 'nimsuggest-check-vervosity "0.1.0")

;; flycheck-nimsuggest
(defvar nim-use-flycheck-nimsuggest t
  "Set nil if you really don’t want to use flycheck-nimsuggest.
Mainly this variable is debug purpose.")

(provide 'nim-vars)
;;; nim-vars.el ends here
