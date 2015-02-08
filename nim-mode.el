;;; nim-mode.el --- A major mode for the Nim programming language
;;
;; Filename: nim-mode.el
;; Description: A major mode for the Nim programming language
;; Author: Simon Hafner
;; Maintainer: Simon Hafner <hafnersimon@gmail.com>
;; Version: 0.1.5
;; Keywords: nim languages
;; Compatibility: GNU Emacs 24
;; Package-Requires: ((emacs "24"))
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
;;; Commentary:
;;
;; Large parts of this code is shamelessly stolen from python.el and
;; adapted to Nim
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

(eval-and-compile
  (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Helpers                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nim-glue-strings (glue strings)
  "Concatenate some GLUE and a list of STRINGS."
  (mapconcat 'identity strings glue))

(defun nim-regexp-choice (strings)
  "Construct a regexp multiple-choice from a list of STRINGS."
  (concat "\\(" (nim-glue-strings "\\|" strings) "\\)"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Simple keywords                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Define keywords, etc.
;; ---------------------

(eval-and-compile
  (defconst nim-keywords
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
Magic functions."
    )

  (defconst nim-operators
    '( "`" "{." ".}" "[" "]" "{" "}" "(" ")" )
    "Nim standard operators."))


;; Custom faces
;; ------------

(defgroup nim nil
  "A major mode for the Nim programming language."
  :link '(url-link "http://nim-lang.org/")
  :group 'languages)

;; TODO: make work!?
(defface nim-tab-face
  '((((class color) (background dark))
     (:background "grey22" :foreground "darkgray"))
    (((class color) (background light))
     (:background "beige"  :foreground "lightgray"))
    (t (:inverse-video t)))
  "Face used to visualize TAB."
  :group 'nim)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Nim specialized rx                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-and-compile 
  (defconst nim-rx-constituents
    `((keyword . ,(rx symbol-start (eval (cons 'or nim-keywords)) symbol-end))
      (type . ,(rx symbol-start (eval (cons 'or nim-types)) symbol-end))
      (exception . ,(rx symbol-start (eval (cons 'or nim-exceptions)) symbol-end))
      (constant . ,(rx symbol-start (eval (cons 'or nim-constants)) symbol-end))
      (builtin . ,(rx symbol-start (eval (cons 'or nim-builtins)) symbol-end))
      (decl-block . ,(rx symbol-start
                         (or "type" "const" "var" "let")
                         symbol-end
                         (* space)
                         (or "#" eol)))
      (block-start          . ,(rx (or (and symbol-start
                                            (or "type" "const" "var" "let")
                                            symbol-end
                                            (* space)
                                            (or "#" eol))
                                       (and symbol-start
                                            (or "proc" "method" "converter" "iterator"
                                                "template" "macro"
                                                "if" "elif" "else" "when" "while" "for"
                                                "try" "except" "finally"
                                                "with" "block"
                                                "enum" "tuple" "object")
                                            symbol-end))))
      (defun                 . ,(rx symbol-start
                                    (or "proc" "method" "converter"
                                        "iterator" "template" "macro")
                                    symbol-end))
      (symbol-name          . ,(rx (any letter ?_) (* (any word ?_))))
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
                                (group (or  "\"" "\"\"\"" "'" "'''"))))))
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

  (defconst nim-font-lock-keywords
    `(  ;; note the BACKTICK, `
      ;; (,(nim-rx (1+ "\t")) . nim-tab-face) ;; TODO: make work!
      (,(nim-rx defun (1+ whitespace) (group symbol-name))
       . (1 font-lock-function-name-face))
      (,(nim-rx (or "var" "let") (1+ whitespace) (group symbol-name))
       . (1 font-lock-variable-name-face))
      (,(nim-rx (or exception type)) . font-lock-type-face)
      (,(nim-rx constant) . font-lock-constant-face)
      (,(nim-rx builtin) . font-lock-builtin-face)
      (,(nim-rx keyword) . font-lock-keyword-face)
      (,(nim-rx "{." (1+ any) ".}") . font-lock-preprocessor-face)
      (,(nim-rx symbol-name (* whitespace) ":" (* whitespace) (group symbol-name))
       . (1 font-lock-type-face))
      )
    "Font lock expressions for Nim mode.")

  (put 'nim-mode 'font-lock-defaults '(nim-font-lock-keywords nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Indentation                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst nim-indent-offset 2 "Number of spaces per level of indentation.")

;;; Indentation

(defvar nim-indent-current-level 0
  "Current indentation level `nim-indent-line-function' is using.")

(defvar nim-indent-levels '(0)
  "Levels of indentation available for `nim-indent-line-function'.")

(defvar nim-indent-indenters
  (nim-rx (or "type" "const" "var" "let" "tuple" "object" "enum" ":"
                 (and defun (* (not (any ?=))) "=")
                 (and "object" (+ whitespace) "of" (+ whitespace) symbol-name)))
  "Regular expression matching the end of line after with a block starts.
If the end of a line matches this regular expression, the next
line is considered an indented block.  Whitespaces at the end of a
line are ignored.")

(defvar nim-indent-dedenters
  (nim-rx symbol-start
             (or "else" "elif" "finally" "except")
             symbol-end
             (* (not (in "\n")))
             ":" (* space) (or "#" eol))
  "Regular expression matching the end of line after which should be dedented.
If the end of a line matches this regular expression, the line
will be dedented relative to the previous block.")

(defcustom nim-indent-trigger-commands
  '(indent-for-tab-command yas-expand yas/expand)
  "Commands that might trigger a `nim-indent-line' call."
  :type '(repeat symbol)
  :group 'nim)

(defun nim-syntax-context (type &optional syntax-ppss)
  "Return non-nil if point is on TYPE using SYNTAX-PPSS.
TYPE can be `comment', `string' or `paren'.  It returns the start
character address of the specified TYPE."
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (cl-case type
      (comment (and (nth 4 ppss) (nth 8 ppss)))
      (string (and (not (nth 4 ppss)) (nth 8 ppss)))
      (paren (nth 1 ppss))
      (t nil))))

(defun nim-syntax-context-type (&optional syntax-ppss)
  "Return the context type using SYNTAX-PPSS.
The type returned can be `comment', `string' or `paren'."
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (cond
     ((nth 8 ppss) (if (nth 4 ppss) 'comment 'string))
     ((nth 1 ppss) 'paren))))

(defsubst nim-syntax-comment-or-string-p (&optional syntax-ppss)
  "Return non-nil if point is inside 'comment or 'string.
Use the parser state at point or SYNTAX-PPSS."
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (nth 8 ppss)))

(defun nim-indent-context ()
  "Get information on indentation context.
Context information is returned with a cons with the form:
    \(STATUS . START)

Where status can be any of the following symbols:
 * inside-paren: If point in between (), {} or [].  START is the
   position of the opening parenthesis.
 * inside-string: If point is inside a string.  START is the
   beginning of the string.
 * after-beginning-of-block: Point is after beginning of
   block.  START is the beginning position of line that starts the
   new block.
 * after-operator: Previous line ends in an operator or current
   line starts with an operator.  START is the position of the
   operator.
 * after-line: Point is after normal line.  START is the beginning
   of the line.
 * no-indent: Point is at beginning of buffer or other special
   case.  START is the position of point."
  (save-restriction
    (widen)
    ;; restrict to the enclosing parentheses, if any
    (let* ((within-paren (nim-util-narrow-to-paren))
           (ppss (save-excursion (beginning-of-line) (syntax-ppss)))
           (start))
      (cons
       (cond
        ;; Beginning of buffer
        ((= (line-beginning-position) (point-min))
         (setq start (point))
         (if within-paren 'inside-paren 'no-indent))
        ;; Inside string
        ((setq start (nim-syntax-context 'string ppss))
         'inside-string)
        ;; After beginning of block
        ((setq start (save-excursion
                       (when (progn
                               (back-to-indentation)
                               (nim-util-forward-comment -1)
                               (or (save-excursion
                                     (back-to-indentation)
                                     (looking-at (nim-rx decl-block)))
                                   (memq (char-before) '(?: ?=))
                                   (looking-back nim-indent-indenters)))
                         (cond
                          ((= (char-before) ?:)
                           (nim-util-backward-stmt)
                           (point))
                          ((= (char-before) ?=)
                           (nim-util-backward-stmt)
                           (and (looking-at (nim-rx defun))
                                (point)))
                          ;; a single block statement on a line like type, var, const, ...
                          (t
                           (back-to-indentation)
                           (point))))))
         'after-beginning-of-block)
        ;; Current line begins with operator
        ((setq start (save-excursion
                       (progn
                         (back-to-indentation)
                         (and (looking-at (nim-rx operator))
                              (match-beginning 0)))))
         'after-operator)
        ;; After operator on previous line
        ((setq start (save-excursion
                       (progn
                         (back-to-indentation)
                         (nim-util-forward-comment -1)
                         (and (looking-back (nim-rx operator)
                                            (line-beginning-position))
                              (match-beginning 0)))))
         'after-operator)
        ;; After normal line
        ((setq start (save-excursion
                       (back-to-indentation)
                       (point)))
         (if (and within-paren
                  (save-excursion
                    (skip-chars-backward "\s\n")
                    (bobp)))
             'inside-paren
           'after-line))
        ;; Do not indent
        (t 'no-indent))
       start))))

(defun nim-indent-calculate-indentation ()
  "Calculate correct indentation offset for the current line."
  (let* ((indentation-context (nim-indent-context))
         (context-status (car indentation-context))
         (context-start (cdr indentation-context)))
    (save-restriction
      (widen)
      ;; restrict to enclosing parentheses, if any
      (nim-util-narrow-to-paren)
      (save-excursion
        (cl-case context-status
          ('no-indent 0)
          ;; When point is after beginning of block just add one level
          ;; of indentation relative to the context-start
          ('after-beginning-of-block
           (goto-char context-start)
           (+ (nim-util-real-current-column) nim-indent-offset))
          ;; When after a simple line just use previous line
          ;; indentation, in the case current line starts with a
          ;; `nim-indent-dedenters' de-indent one level.
          ('after-line
           (-
            (save-excursion
              (goto-char context-start)
              (forward-line -1)
              (end-of-line)
              (nim-nav-beginning-of-statement)
              (nim-util-real-current-indentation))
            (if (progn
                  (back-to-indentation)
                  (looking-at nim-indent-dedenters))
                nim-indent-offset
              0)))
          ;; When inside of a string, do nothing. just use the current
          ;; indentation.  XXX: perhaps it would be a good idea to
          ;; invoke standard text indentation here
          ('inside-string
           (goto-char context-start)
           (nim-util-real-current-indentation))
          ;; When point is after an operator line, there are several cases
          ('after-operator
           (save-excursion
             (nim-nav-beginning-of-statement)
             (cond
              ;; current line is a continuation of a block statement
              ((looking-at (nim-rx block-start (* space)))
               (goto-char (match-end 0))
               (nim-util-real-current-column))
              ;; current line is a continuation of an assignment
              ;; operator. Find an assignment operator that is not
              ;; contained in a string/comment/paren and is not
              ;; followed by whitespace only
              ((save-excursion
                 (and (re-search-forward (nim-rx not-simple-operator
                                                    assignment-operator
                                                    not-simple-operator)
                                         nil
                                         t)
                      (not (nim-syntax-context-type))
                      (progn
                        (backward-char)
                        (not (looking-at (rx (* space) (or "#" eol)))))))
               (goto-char (match-end 0))
               (skip-syntax-forward "\s")
               (nim-util-real-current-column))
              ;; current line is a continuation of some other operator, just indent
              (t
               (back-to-indentation)
               (+ (nim-util-real-current-column) nim-indent-offset)))))
          ;; When inside a paren there's a need to handle nesting
          ;; correctly
          ('inside-paren
           (cond
            ;; If current line closes the outermost open paren use the
            ;; current indentation of the context-start line.
            ((save-excursion
               (skip-syntax-forward "\s" (line-end-position))
               (when (and (looking-at (nim-rx (in ")]}")))
                          (progn
                            (forward-char 1)
                            (not (nim-syntax-context 'paren))))
                 (goto-char context-start)
                 (nim-util-real-current-indentation))))
            ;; If open paren is contained on a line by itself add another
            ;; indentation level, else look for the first word after the
            ;; opening paren and use it's column position as indentation
            ;; level.
            ((let* ((content-starts-in-newline)
                    (indent
                     (save-excursion
                       (if (setq content-starts-in-newline
                                 (progn
                                   (goto-char context-start)
                                   (forward-char)
                                   (save-restriction
                                     (narrow-to-region
                                      (line-beginning-position)
                                      (line-end-position))
                                     (nim-util-forward-comment))
                                   (looking-at "$")))
                           (+ (nim-util-real-current-indentation) nim-indent-offset)
                         (nim-util-real-current-column)))))
               ;; Adjustments
               (cond
                ;; If current line closes a nested open paren de-indent one
                ;; level.
                ((progn
                   (back-to-indentation)
                   (looking-at (nim-rx ")]}")))
                 (- indent nim-indent-offset))
                ;; If the line of the opening paren that wraps the current
                ;; line starts a block add another level of indentation to
                ;; follow new pep8 recommendation. See: http://ur1.ca/5rojx
                ((save-excursion
                   (when (and content-starts-in-newline
                              (progn
                                (goto-char context-start)
                                (back-to-indentation)
                                (looking-at (nim-rx block-start))))
                     (+ indent nim-indent-offset))))
                (t indent)))))))))))

(defun nim-indent-calculate-levels ()
  "Calculate `nim-indent-levels' and reset `nim-indent-current-level'."
  (let* ((indentation (nim-indent-calculate-indentation))
         (remainder (% indentation nim-indent-offset))
         (steps (/ (- indentation remainder) nim-indent-offset)))
    (setq nim-indent-levels (list 0))
    (dotimes (step steps)
      (push (* nim-indent-offset (1+ step)) nim-indent-levels))
    (when (not (eq 0 remainder))
      (push (+ (* nim-indent-offset steps) remainder) nim-indent-levels))
    (setq nim-indent-levels (nreverse nim-indent-levels))
    (setq nim-indent-current-level (1- (length nim-indent-levels)))))

(defun nim-indent-toggle-levels ()
  "Toggle `nim-indent-current-level' over `nim-indent-levels'."
  (setq nim-indent-current-level (1- nim-indent-current-level))
  (when (< nim-indent-current-level 0)
    (setq nim-indent-current-level (1- (length nim-indent-levels)))))

(defun nim-indent-line (&optional force-toggle)
  "Internal implementation of `nim-indent-line-function'.
Uses the offset calculated in
`nim-indent-calculate-indentation' and available levels
indicated by the variable `nim-indent-levels' to set the
current indentation.

When the variable `last-command' is equal to one of the symbols
inside `nim-indent-trigger-commands' or FORCE-TOGGLE is
non-nil it cycles levels indicated in the variable
`nim-indent-levels' by setting the current level in the
variable `nim-indent-current-level'.

When the variable `last-command' is not equal to one of the
symbols inside `nim-indent-trigger-commands' and FORCE-TOGGLE
is nil it calculates possible indentation levels and saves it in
the variable `nim-indent-levels'.  Afterwards it sets the
variable `nim-indent-current-level' correctly so offset is
equal to (`nth' `nim-indent-current-level'
`nim-indent-levels')"
  (or
   (and (or (and (memq this-command nim-indent-trigger-commands)
                 (eq last-command this-command))
            force-toggle)
        (not (equal nim-indent-levels '(0)))
        (or (nim-indent-toggle-levels) t))
   (nim-indent-calculate-levels))
  (let* ((starting-pos (point-marker))
         (indent-ending-position
          (+ (line-beginning-position) (current-indentation)))
         (follow-indentation-p
          (or (bolp)
              (and (<= (line-beginning-position) starting-pos)
                   (>= indent-ending-position starting-pos))))
         (next-indent (nth nim-indent-current-level nim-indent-levels)))
    (unless (= next-indent (current-indentation))
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-to next-indent)
      (goto-char starting-pos))
    (and follow-indentation-p (back-to-indentation)))
  ;(nim-info-closing-block-message)
  )

(defun nim-indent-line-function ()
  "`indent-line-function' for Nim mode.
See `nim-indent-line' for details."
  (nim-indent-line))

(defun nim-indent-dedent-line ()
  "De-indent current line."
  (interactive "*")
  (when (and (not (nim-syntax-comment-or-string-p))
             (<= (point-marker) (save-excursion
                                  (back-to-indentation)
                                  (point-marker)))
             (> (current-column) 0))
    (nim-indent-line t)
    t))

(defun nim-indent-dedent-line-backspace (arg)
  "De-indent current line.
Argument ARG is passed to `backward-delete-char-untabify' when
point is  not in between the indentation."
  (interactive "*p")
  (when (not (nim-indent-dedent-line))
    (backward-delete-char-untabify arg)))
(put 'nim-indent-dedent-line-backspace 'delete-selection 'supersede)

(defun nim-indent-region (start end)
  "Indent a nim region automagically.

Called from a program, START and END specify the region to indent."
  (let ((deactivate-mark nil))
    (save-excursion
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (or (bolp) (forward-line 1))
      (while (< (point) end)
        (or (and (bolp) (eolp))
            (let (word)
              (forward-line -1)
              (back-to-indentation)
              (setq word (current-word))
              (forward-line 1)
              (when (and word
                         ;; Don't mess with strings, unless it's the
                         ;; enclosing set of quotes.
                         (or (not (nim-syntax-context 'string))
                             (eq
                              (syntax-after
                               (+ (1- (point))
                                  (current-indentation)
                                  (nim-syntax-count-quotes (char-after) (point))))
                              (string-to-syntax "|"))))
                (beginning-of-line)
                (delete-horizontal-space)
                (indent-to (nim-indent-calculate-indentation)))))
        (forward-line 1))
      (move-marker end nil))))

(defun nim-indent-shift-left (start end &optional count)
  "Shift lines contained in region START END by COUNT columns to the left.
COUNT defaults to `nim-indent-offset'.  If region isn't
active, the current line is shifted.  The shifted region includes
the lines in which START and END lie.  An error is signaled if
any lines in the region are indented less than COUNT columns."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (if count
      (setq count (prefix-numeric-value count))
    (setq count nim-indent-offset))
  (when (> count 0)
    (let ((deactivate-mark nil))
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (if (and (< (current-indentation) count)
                   (not (looking-at "[ \t]*$")))
              (error "Can't shift all lines enough"))
          (forward-line))
        (indent-rigidly start end (- count))))))

(add-to-list 'debug-ignored-errors "^Can't shift all lines enough")

(defun nim-indent-shift-right (start end &optional count)
  "Shift lines contained in region START END by COUNT columns to the left.
COUNT defaults to `nim-indent-offset'.  If region isn't
active, the current line is shifted.  The shifted region includes
the lines in which START and END lie."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (let ((deactivate-mark nil))
    (if count
        (setq count (prefix-numeric-value count))
      (setq count nim-indent-offset))
    (indent-rigidly start end count)))

(defun nim-indent-electric-colon (arg)
  "Insert a colon and maybe de-indent the current line.
With numeric ARG, just insert that many colons.  With
\\[universal-argument], just insert a single colon."
  (interactive "*P")
  (self-insert-command (if (not (integerp arg)) 1 arg))
  (when (and (not arg)
             (eolp)
             (not (equal ?: (char-after (- (point-marker) 2))))
             (not (nim-syntax-comment-or-string-p)))
    (let ((indentation (current-indentation))
          (calculated-indentation (nim-indent-calculate-indentation)))
      ;(nim-info-closing-block-message)
      (when (> indentation calculated-indentation)
        (save-excursion
          (indent-line-to calculated-indentation)
          ;; (when (not (nim-info-closing-block-message))
          ;;   (indent-line-to indentation)))))))
          )))))
(put 'nim-indent-electric-colon 'delete-selection t)

(defun nim-indent-post-self-insert-function ()
  "Adjust closing paren line indentation after a char is added.
This function is intended to be added to the
`post-self-insert-hook.'  If a line renders a paren alone, after
adding a char before it, the line will be re-indented
automatically if needed."
  (when (and (eq (char-before) last-command-event)
             (not (bolp))
             (memq (char-after) '(?\) ?\] ?\})))
    (save-excursion
      (goto-char (line-beginning-position))
      ;; If after going to the beginning of line the point
      ;; is still inside a paren it's ok to do the trick
      (when (nim-syntax-context 'paren)
        (let ((indentation (nim-indent-calculate-indentation)))
          (when (< (current-indentation) indentation)
            (indent-line-to indentation)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Utility functions ...                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nim-util-forward-comment (&optional direction)
  "Nim mode specific version of `forward-comment'.
Optional argument DIRECTION defines the direction to move to."
  (let ((comment-start (nim-syntax-context 'comment))
        (factor (if (< (or direction 0) 0)
                    -99999
                  99999)))
    (when comment-start
      (goto-char comment-start))
    (forward-comment factor)))

(defun nim-util-backward-stmt ()
  "Move point backward to the beginning of the current statement.
Point is moved to the beginning of the first symbol that is
either the first on a line or the first after a
semicolon.  Balanced parentheses, strings and comments are
skipped."
  (let ((level (nth 0 (syntax-ppss))))
    (save-restriction
      ;; narrow to surrounding parentheses
      (nim-util-narrow-to-paren)
      (while (progn
               (if (re-search-backward "[,;]" (line-beginning-position) t)
                   (forward-char)
                 (beginning-of-line))
               (let ((state (syntax-ppss)))
                 (and
                  (or (> (nth 0 state) level)
                      (nim-syntax-comment-or-string-p state)
                      (save-match-data
                        (looking-at (nim-rx (* space) (group operator))))
                      (not (looking-at (nim-rx (* space) (group symbol-name)))))
                  (not (bobp))
                  (prog1 t (backward-char))))))
      (and (match-beginning 1)
           (goto-char (match-beginning 1))))))

(defun nim-util-narrow-to-paren ()
  "Narrow buffer to content of enclosing parentheses.
Returns non-nil if and only if there are enclosing parentheses."
  (save-excursion
    (condition-case nil
        (prog1 t
          (narrow-to-region (progn
                              (backward-up-list)
                              (1+ (point)))
                            (progn
                              (forward-list)
                              (1- (point)))))
      (scan-error nil))))

(defun nim-util-real-current-column ()
  "Return the current column without narrowing."
  (+ (current-column)
     (if (= (line-beginning-position) (point-min))
         (save-excursion
           (goto-char (point-min))
           (save-restriction
             (widen)
             (current-column)))
       0)))

(defun nim-util-real-current-indentation ()
  "Return the indentation without narrowing."
  (+ (current-indentation)
     (if (= (line-beginning-position) (point-min))
         (save-excursion
           (goto-char (point-min))
           (save-restriction
             (widen)
             (current-column)))
       0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Navigation functions ...                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nim-nav-beginning-of-statement ()
  "Move to start of current statement."
  (interactive "^")
  (while (and (nim-util-backward-stmt)
              (not (bobp))
              (memq (car (nim-indent-context))
                    '(after-operator)))
    (end-of-line 0))
  (point))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Wrap it all up ...                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar nim-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.") 'nim-goto-sym)
    (define-key map (kbd "C-c h") 'nim-explain-sym)
    (define-key map ":" 'nim-indent-electric-colon)
    (define-key map "\C-c<" 'nim-indent-shift-left)
    (define-key map "\C-c>" 'nim-indent-shift-right)
    map))

;;;###autoload
(define-derived-mode nim-mode prog-mode "Nim"
  "A major mode for the Nim programming language."
  :group 'nim

  (setq font-lock-defaults '(nim-font-lock-keywords nil t))

  ;; modify the keymap
  (set (make-local-variable 'indent-line-function) 'nim-indent-line-function)
  (set (make-local-variable 'indent-region-function) #'nim-indent-region)

  ;; Documentation comment highlighting
  ;; (modify-syntax-entry ?\# ". 12b" nim-mode-syntax-table)
  ;; (modify-syntax-entry ?\n "> b" nim-mode-syntax-table)

  ;; Comment
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#+\\s-*")

  ;; Comment highlighting
  (modify-syntax-entry ?# "< b"  nim-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" nim-mode-syntax-table)

  (modify-syntax-entry ?\' "w"  nim-mode-syntax-table)
  (modify-syntax-entry ?\" "|"  nim-mode-syntax-table)

  (modify-syntax-entry ?\[ "(]"  nim-mode-syntax-table)
  (modify-syntax-entry ?\] ")["  nim-mode-syntax-table)

  (setq indent-tabs-mode nil) ;; Always indent with SPACES!
  )

(defcustom nim-compiled-buffer-name "*nim-js*"
  "The name of the scratch buffer used to compile Javascript from Nim."
  :type 'string
  :group 'nim)

(defcustom nim-command "nim"
  "Path to the nim executable.
You don't need to set this if the nim executable is inside your PATH."
  :type 'string
  :group 'nim)

(defcustom nim-args-compile '()
  "The arguments to pass to `nim-command' to compile a file."
  :type '(repeat string)
  :group 'nim)

(defvar nim-idetools-modes '(suggest def context usages)
  "Which modes are available to use with the idetools.")

(defun nim-compile-file-to-js (&optional callback)
  "Save current file and compiles it.
Use the project directory, so it will work best with external
libraries where `nim-compile-region-to-js' does not.  Return the
filename of the compiled file.  The CALLBACK is executed on
success with the filename of the compiled file."
  (interactive)
  (save-buffer)
  (let ((default-directory (or (nim-get-project-root) default-directory)))
    (lexical-let ((callback callback))
      (nim-compile (list "js" (buffer-file-name))
                      (lambda () (when callback
                              (funcall callback (concat default-directory
                                                        "nimcache/"
                                                        (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
                                                        ".js"))))))))

(defun nim-compile-region-to-js (start end)
  "Compile the current region to javascript.
The result is written into the buffer
`nim-compiled-buffer-name'."
  (interactive "r")

  (lexical-let ((buffer (get-buffer-create nim-compiled-buffer-name))
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

(defun nim-compile (args &optional on-success)
  "Invoke the compiler and call ON-SUCCESS in case of successful compilation."
  (lexical-let ((on-success (or on-success (lambda () (message "Compilation successful.")))))
    (if (bufferp "*nim-compile*")
        (with-current-buffer "*nim-compile*"
          (erase-buffer)))
    (set-process-sentinel
     (apply
      (apply-partially 'start-file-process "nim" "*nim-compile*" nim-command)
      (append nim-args-compile args))
     (lambda (process-name status)
       (cond ((string= status "finished\n")
              (when on-success
                (funcall on-success)))
             ((string= status "exited abnormally with code 1\n")
              (display-buffer "*nim-compile*"))
             (t (error status)))))))

(defun nim-call-and-parse-idetools (mode)
  "Call idetools and get `nim-ide' structs back."
  (nim-parse-idetools-buffer (nim-call-idetools mode)))

(cl-defstruct nim-ide type namespace name signature path line column comment)

(defun nim-parse-idetools-buffer (buffer)
  "Return a list of `nim-ide' structs, based on the contents of BUFFER."
  (with-current-buffer buffer
    (mapcar (lambda (line)
              (destructuring-bind (_ type fn sig path line col comment) (split-string line "\t")
                (string-match "^\\(?:\\(.*\\)\\.\\)?\\([^.]*\\)$" fn)
                (make-nim-ide
                 :type type
                 :namespace (match-string 1 fn)
                 :name (match-string 2 fn)
                 :signature sig
                 :path path
                 :line (string-to-number line)
                 :column (string-to-number col)
                 :comment comment)))
            (split-string (buffer-string) "[\r\n]" t))))

(defun nim-call-idetools (mode)
  "Grab the data from the returned buffer.
MODE should be one of `nim-idetools-modes'."
  (when (not (memq mode nim-idetools-modes))
    (error "Mode %s not one from `nim-idetools-modes'" mode))
  (let ((tempfile (nim-save-buffer-temporarly))
        (file (buffer-file-name))
        (buffer (get-buffer-create (format "*nim-idetools-%s*" mode))))
    ;; There can only be one. Useful for suggest, not sure about the
    ;; other modes. Change as needed.
    (when (bufferp buffer)
      (with-current-buffer buffer
        (erase-buffer)))
    (let ((args (append (list nim-command nil (list buffer (concat temporary-file-directory "nim-idetools-stderr")) nil)
                   (remove nil (list
                                "idetools"
                                "--stdout"
                                (nim-format-cursor-position file tempfile) ; --trackDirty
                                (when (nim-get-project-root)
                                  (format "--include:%s" (nim-get-project-root)))
                                (concat "--" (symbol-name mode))
                                ;; in case of no project main file, use the tempfile. Might be
                                ;; useful for repl.
                                (or (nim-get-project-main-file) tempfile))))))
      ;; (message (format "%S" args))      ; Debugging
      (apply 'call-process args))
    (delete-directory (file-name-directory tempfile) t)
    buffer))

(defun nim-save-buffer-temporarly ()
  "Save the current buffer and return the location, so we
can pass it to idetools."
  (let* ((dirname (make-temp-file "nim-suggest" t))
         (filename (concat (file-name-as-directory dirname)
                           (file-name-nondirectory (buffer-file-name)))))
    (save-restriction
      (widen)
      (write-region (point-min) (point-max) filename) nil 'foo)
    filename))

;; From http://stackoverflow.com/questions/14095189/walk-up-the-directory-tree

(defun nim-parent-directory (dir)
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))

(defun nim-find-file-in-heirarchy (current-dir pattern)
  "Search for a file matching PATTERN upwards through the directory
hierarchy, starting from CURRENT-DIR"
  (let ((parent (nim-parent-directory (expand-file-name current-dir))))
    (or (directory-files current-dir t pattern nil)
      (when parent
        (nim-find-file-in-heirarchy parent pattern)))))

(defun nim-get-project-main-file ()
  "Get the main file for the project."
  (let ((main-file (nim-find-file-in-heirarchy
                (file-name-directory (buffer-file-name))
                ".*\.nim\.cfg")))
    (when main-file (concat
                     (replace-regexp-in-string "\.nim\.cfg$" "" (first main-file))
                     ".nim"))))

(defun nim-get-project-root ()
  "Get the project root.
Uses `nim-get-project-main-file' or git."
  (or (let ((main-file (nim-get-project-main-file)))
        (when main-file (file-name-directory main-file)))
      (let ((git-output (replace-regexp-in-string "\n$" ""
                                        (with-output-to-string
                                          (with-current-buffer
                                              standard-output
                                            (process-file shell-file-name nil (list t nil) nil shell-command-switch "git rev-parse --show-toplevel"))))))
        (if (< 0 (length git-output))
            git-output
          nil))))

(defun nim-format-cursor-position (file tempfile)
  "Format the position of the cursor to a nice little
--trackDirty statement, referencing the FILE in the TEMPFILE
directory."
  (format "--trackDirty:%s,%s,%d,%d" tempfile file (line-number-at-pos) (current-column)))

(defun nim-goto-sym ()
  "Go to the definition of the symbol currently under the cursor."
  (interactive)
  (let ((def (first (nim-call-and-parse-idetools 'def))))
    (when (not def) (error "Symbol not found"))
    (find-file (nim-ide-path def))
    (goto-char (point-min))
    (forward-line (1- (nim-ide-line def)))))

;; compilation error
(eval-after-load 'compile
  '(progn
     (add-to-list 'compilation-error-regexp-alist 'nim)
     (add-to-list 'compilation-error-regexp-alist-alist
                  '(nim "^\\s-*\\(.*\\)(\\([0-9]+\\),\\s-*\\([0-9]+\\))\\s-+\\(?:Error\\|\\(Hint\\)\\):" 1 2 3 (4)))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.nim\\'" . nim-mode))

(provide 'nim-mode)

;;; nim-mode.el ends here
