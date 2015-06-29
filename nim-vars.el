;;; nim-vars.el --- nim-mode's variables

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

;; TODO: make work!?
(defface nim-tab-face
  '((((class color) (background dark))
     (:background "grey22" :foreground "darkgray"))
    (((class color) (background light))
     (:background "beige"  :foreground "lightgray"))
    (t (:inverse-video t)))
  "Face used to visualize TAB."
  :group 'nim)

(defconst nim-indent-offset 2 "Number of spaces per level of indentation.")

;; Keywords
(defconst nim-keywords
  '("addr" "and" "as" "asm" "atomic" "bind" "block" "break" "case"
    "cast" "const" "continue" "converter" "discard" "distinct" "div" "do"
    "elif" "else" "end" "enum" "except" "export" "finally" "for" "from"
    "generic" "if" "import" "in" "include" "interface" "is" "isnot"
    "iterator" "lambda" "let" "macro" "method" "mixin" "mod" "nil" "not"
    "notin" "object" "of" "or" "out" "proc" "ptr" "raise" "ref" "return"
    "shared" "shl" "shr" "static" "template" "try" "tuple" "type" "var"
    "when" "while" "with" "without" "xor" "yield")
  "Nim keywords.
The above string is taken from URL
`http://nim-lang.org/manual.html#identifiers-keywords', for easy
updating.")

(defconst nim-types
  '("int" "int8" "int16" "int32" "int64" "float" "float32" "float64"
    "bool" "char" "string" "cstring" "pointer" "ordinal" "nil" "expr"
    "stmt" "typedesc" "range" "array" "openarray" "seq" "set"
    "tgenericseq" "pgenericseq" "nimstringdesc" "nimstring" "byte"
    "natural" "positive" "tobject" "pobject" "tresult" "tendian"
    "taddress" "biggestint" "biggestfloat" "cchar" "cschar" "cshort"
    "cint" "clong" "clonglong" "cfloat" "cdouble" "clongdouble"
    "cstringarray" "pfloat32" "pfloat64" "pint64" "pint32"
    "tgc_strategy" "tfile" "tfilemode")
  "Nim types defined in <lib/system.nim>.")

(defconst nim-exceptions
  '("e_base" "easynch" "esynch" "esystem" "eio" "eos"
    "einvalidlibrary" "eresourceexhausted" "earithmetic" "edivbyzero"
    "eoverflow" "eaccessviolation" "eassertionfailed" "econtrolc"
    "einvalidvalue" "eoutofmemory" "einvalidindex" "einvalidfield"
    "eoutofrange" "estackoverflow" "enoexceptiontoreraise"
    "einvalidobjectassignment" "einvalidobjectconversion"
    "efloatingpoint" "efloatinginvalidop" "efloatdivbyzero"
    "efloatoverflow" "efloatunderflow" "efloatinexact")
  "Nim exceptions defined in <lib/system.nim>.")

(defconst nim-constants
  '("ismainmodule" "compiledate" "compiletime" "nimversion"
    "nimmajor" "nimminor" "nimpatch" "cpuendian" "hostos"
    "hostcpu" "apptype" "inf" "neginf" "nan" "quitsuccess"
    "quitfailure" "stdin" "stdout" "stderr" "true" "false" )
  "Nim constants defined in <lib/system.nim>.")

(defconst nim-builtins
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
Magic functions.")

(defconst nim-operators
  '( "`" "{." ".}" "[" "]" "{" "}" "(" ")" )
  "Nim standard operators.")

(defvar nim-rx-constituents
  (let* ((constituents1
          (cl-loop for (sym . kwd) in `((keyword           . ,nim-keywords)
                                        (dedenter          . ("elif" "else" "of" "except" "finally"))
                                        (type              . ,nim-types)
                                        (exception         . ,nim-exceptions)
                                        (constant          . ,nim-constants)
                                        (builtin           . ,nim-builtins)
                                        (defun             . ("proc" "method" "converter" "iterator" "template" "macro"))
                                        (block-ender       . ("break" "continue" "raise" "return"))
                                        (block-start-defun . ("proc" "method" "converter" "iterator"
                                                              "template" "macro"
                                                              "if" "elif" "else" "when" "while" "for" "of"
                                                              "try" "except" "finally"
                                                              "with" "block"
                                                              "enum" "tuple" "object")))
                   collect (cons sym (apply `((lambda () (rx symbol-start (or ,@kwd) symbol-end)))))))
         (constituents2 `((decl-block . ,(rx symbol-start
                                             (or "type" "const" "var" "let" "import")
                                             symbol-end
                                             (* space)
                                             (or "#" eol)))

                          (symbol-name          . ,(rx (any letter ?_ ?–) (* (any word ?_ ?–))))
                          (dec-number . ,(rx symbol-start
                                             (1+ (in digit "_"))
                                             (opt "." (in digit "_"))
                                             (opt (any "eE") (1+ digit))
                                             (opt "'" (or "i8" "i16" "i32" "i64" "f32" "f64"))
                                             symbol-end))
                          (hex-number . ,(rx symbol-start
                                             (1+ (in xdigit "_"))
                                             (opt "." (in xdigit "_"))
                                             (opt (any "eE") (1+ xdigit))
                                             (opt "'" (or "i8" "i16" "i32" "i64" "f32" "f64"))
                                             symbol-end))
                          (oct-number . ,(rx symbol-start
                                             (1+ (in "0-7_"))
                                             (opt "." (in "0-7_"))
                                             (opt (any "eE") (1+ "0-7_"))
                                             (opt "'" (or "i8" "i16" "i32" "i64" "f32" "f64"))
                                             symbol-end))
                          (bin-number . ,(rx symbol-start
                                             (1+ (in "0-1_"))
                                             (opt "." (in "0-1_"))
                                             (opt (any "eE") (1+ "0-1_"))
                                             (opt "'" (or "i8" "i16" "i32" "i64" "f32" "f64"))
                                             symbol-end))
                          (open-paren           . ,(rx (or "{" "[" "(")))
                          (close-paren          . ,(rx (or "}" "]" ")")))
                          (pragma               . ,(rx "{." (1+ any) ".}"))
                          (simple-operator      . ,(rx (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%)))
                          ;; FIXME: rx should support (not simple-operator).
                          (not-simple-operator  . ,(rx
                                                    (not
                                                     (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%))))
                          ;; FIXME: Use regexp-opt.
                          (operator             . ,(rx (or (1+ (in "-=+*/<>@$~&%|!?^.:\\"))
                                                           (and
                                                            symbol-start
                                                            (or
                                                             "and" "or" "not" "xor" "shl"
                                                             "shr" "div" "mod" "in" "notin" "is"
                                                             "isnot")
                                                            symbol-end))))
                          ;; FIXME: Use regexp-opt.
                          (assignment-operator  . ,(rx (* (in "-=+*/<>@$~&%|!?^.:\\")) "="))
                          (string-delimiter . ,(rx (and
                                                    ;; Match even number of backslashes.
                                                    (or (not (any ?\\ ?\' ?\")) point
                                                        ;; Quotes might be preceded by a escaped quote.
                                                        (and (or (not (any ?\\)) point) ?\\
                                                             (* ?\\ ?\\) (any ?\' ?\")))
                                                    (* ?\\ ?\\)
                                                    ;; Match single or triple quotes of any kind.
                                                    (group (or  "\"" "\"\"\"" "'" "'''")))))
                          (coding-cookie . ,(rx line-start ?# (* space)
                                                (or
                                                 ;; # coding=<encoding name>
                                                 (: "coding" (or ?: ?=) (* space) (group-n 1 (+ (or word ?-))))
                                                 ;; # -*- coding: <encoding name> -*-
                                                 (: "-*-" (* space) "coding:" (* space)
                                                    (group-n 1 (+ (or word ?-))) (* space) "-*-")
                                                 ;; # vim: set fileencoding=<encoding name> :
                                                 (: "vim:" (* space) "set" (+ space)
                                                    "fileencoding" (* space) ?= (* space)
                                                    (group-n 1 (+ (or word ?-))) (* space) ":")))))))
    (append constituents1 constituents2))
  "Additional Nim specific sexps for `nim-rx'.")

(defmacro nim-rx (&rest regexps)
    "Nim mode specialized rx macro.
This variant of `rx' supports common nim named REGEXPS."
    (let ((rx-constituents (append nim-rx-constituents rx-constituents)))
      (cond ((null regexps)
             (error "No regexp"))
            ((cdr regexps)
             (rx-to-string `(and ,@regexps) t))
            (t
             (rx-to-string (car regexps) t)))))

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

    ;; exceptions
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?` "$" table)

    ;; Parentheses
    (modify-syntax-entry ?\[ "(]  " table)
    (modify-syntax-entry ?\] ")[  " table)
    (modify-syntax-entry ?\{ "(}  " table)
    (modify-syntax-entry ?\} "){  " table)
    (modify-syntax-entry ?\( "()  " table)
    (modify-syntax-entry ?\) ")(  " table)

    ;; ;; Documentation comment highlighting
    ;; ;; (modify-syntax-entry ?\# ". 12b" nim-mode-syntax-table)
    ;; ;; (modify-syntax-entry ?\n "> b" nim-mode-syntax-table)
    ;; ;; Comment highlighting
    ;; (modify-syntax-entry ?# "< b"  nim-mode-syntax-table)
    ;; (modify-syntax-entry ?\n "> b" nim-mode-syntax-table)
    ;; (modify-syntax-entry ?\' "w"  nim-mode-syntax-table)
    ;; (modify-syntax-entry ?\" "|"  nim-mode-syntax-table)
    table)
  "Syntax table for Nim files.")

(defvar nim-dotty-syntax-table
  (let ((table (make-syntax-table nim-mode-syntax-table)))
    (modify-syntax-entry ?. "w" table)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Dotty syntax table for Nim files.
It makes underscores and dots word constituent chars.")

(provide 'nim-vars)
;;; nim-vars.el ends here
