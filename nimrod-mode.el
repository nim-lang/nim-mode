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
;; Large parts of this code is shamelessly stolen from python.el and
;; adapted to Nimrod
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Nimrod specialized rx                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile
  (defconst nimrod-rx-constituents
    `((keyword . ,(rx symbol-start (eval (cons 'or nimrod-keywords)) symbol-end))
      (type . ,(rx symbol-start (eval (cons 'or nimrod-types)) symbol-end))
      (exception . ,(rx symbol-start (eval (cons 'or nimrod-exceptions)) symbol-end))
      (constant . ,(rx symbol-start (eval (cons 'or nimrod-constants)) symbol-end))
      (builtin . ,(rx symbol-start (eval (cons 'or nimrod-builtins)) symbol-end))
      (block-start          . ,(rx symbol-start
                                   (or "type" "const" "var" "let"
                                       "proc" "method" "converter" "iterator"
                                       "template" "macro"
                                       "if" "elif" "else" "when" "while" "for"
                                       "try" "except" "finally"
                                       "with" "block"
                                       "enum" "tuple" "object")
                                   symbol-end))
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
      (operator             . ,(rx (or "+" "-" "/" "&" "^" "~" "|" "*" "<" ">"
                                       "=" "%" "**" "//" "<<" ">>" "<=" "!="
                                       "==" ">=" "is" "not")))
      ;; FIXME: Use regexp-opt.
      (assignment-operator  . ,(rx (or "=" "+=" "-=" "*=" "/=" "//=" "%=" "**="
                                       ">>=" "<<=" "&=" "^=" "|=")))
      (string-delimiter . ,(rx (and
                                ;; Match even number of backslashes.
                                (or (not (any ?\\ ?\' ?\")) point
                                    ;; Quotes might be preceded by a escaped quote.
                                    (and (or (not (any ?\\)) point) ?\\
                                         (* ?\\ ?\\) (any ?\' ?\")))
                                (* ?\\ ?\\)
                                ;; Match single or triple quotes of any kind.
                                (group (or  "\"" "\"\"\"" "'" "'''"))))))
    "Additional Nimrod specific sexps for `nimrod-rx'")

  (defmacro nimrod-rx (&rest regexps)
    "Nimrod mode specialized rx macro.
This variant of `rx' supports common nimrod named REGEXPS."
    (let ((rx-constituents (append nimrod-rx-constituents rx-constituents)))
      (cond ((null regexps)
             (error "No regexp"))
            ((cdr regexps)
             (rx-to-string `(and ,@regexps) t))
            (t
             (rx-to-string (car regexps) t))))))

;; Free memory
(defvar nimrod-keywords nil)
(defvar nimrod-types nil)
(defvar nimrod-exceptions nil)
(defvar nimrod-constants nil)
(defvar nimrod-builtins nil)
(defvar nimrod-operators nil)


(defconst nimrod-font-lock-keywords
  `(  ;; note the BACKTICK, `
    ;; (,(nimrod-rx (1+ "\t")) . nimrod-tab-face) ;; TODO: make work!
    (,(nimrod-rx defun (1+ whitespace) (group symbol-name))
     . (1 font-lock-function-name-face))
    (,(nimrod-rx (or "var" "let") (1+ whitespace) (group symbol-name))
     . (1 font-lock-variable-name-face))
    (,(nimrod-rx (or exception type)) . font-lock-type-face)
    (,(nimrod-rx constant) . font-lock-constant-face)
    (,(nimrod-rx builtin) . font-lock-builtin-face)
    (,(nimrod-rx keyword) . font-lock-keyword-face)
    (,(nimrod-rx "{." (1+ any) ".}") . font-lock-preprocessor-face)
    (,(nimrod-rx symbol-name (* whitespace) ":" (* whitespace) (group symbol-name))
     . (1 font-lock-type-face))
    )
  "Font lock expressions for Nimrod mode.")
(put 'nimrod-mode 'font-lock-defaults '(nimrod-font-lock-keywords nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Indentation                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst nimrod-indent-offset 2 "Number of spaces per level of indentation.")

;;; Indentation

(defvar nimrod-indent-indenters
  (nimrod-rx (or "type" "const" "var" "let" "tuple" "object" ":"
                 (and defun (* (not (any ?=))) "=")
                 (and "object" (+ whitespace) "of" (+ whitespace) symbol-name)))
  "Regular expression matching the end of line after with a block starts.
If the end of a line matches this regular expression, the next
line is considered an indented block. Whitespaces at the end of a
line are ignored.")

(defvar nimrod-indent-dedenters
  (nimrod-rx (or "else" "elif" "finally" "except")
             (* whitespace) ":")
  "Regular expression matching the end of line after which should be dedented.
If the end of a line matches this regular expression, the line
will be detended relative to the previous block.")

(defcustom nimrod-indent-trigger-commands
  '(indent-for-tab-command yas-expand yas/expand)
  "Commands that might trigger a `nimrod-indent-line' call."
  :type '(repeat symbol)
  :group 'nimrod)

(defun nimrod-syntax-context (type &optional syntax-ppss)
  "Return non-nil if point is on TYPE using SYNTAX-PPSS.
TYPE can be `comment', `string' or `paren'.  It returns the start
character address of the specified TYPE."
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (case type
      (comment (and (nth 4 ppss) (nth 8 ppss)))
      (string (and (not (nth 4 ppss)) (nth 8 ppss)))
      (paren (nth 1 ppss))
      (t nil))))

(defun nimrod-syntax-context-type (&optional syntax-ppss)
  "Return the context type using SYNTAX-PPSS.
The type returned can be `comment', `string' or `paren'."
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (cond
     ((nth 8 ppss) (if (nth 4 ppss) 'comment 'string))
     ((nth 1 ppss) 'paren))))

(defsubst nimrod-syntax-comment-or-string-p ()
  "Return non-nil if point is inside 'comment or 'string."
  (nth 8 (syntax-ppss)))

(defun nimrod-indent-context ()
  "Get information on indentation context.
Context information is returned with a cons with the form:
    \(STATUS . START)

Where status can be any of the following symbols:
 * inside-paren: If point in between (), {} or []
 * inside-string: If point is inside a string
 * after-beginning-of-block: Point is after beginning of block
 * after-line: Point is after normal line
 * no-indent: Point is at beginning of buffer or other special case
START is the buffer position where the sexp starts."
  (save-restriction
    (widen)
    (let ((ppss (save-excursion (beginning-of-line) (syntax-ppss)))
          (start))
      (cons
       (cond
        ;; Beginning of buffer
        ((save-excursion
           (goto-char (line-beginning-position))
           (bobp))
         'no-indent)
        ;; Inside string
        ((setq start (nimrod-syntax-context 'string ppss))
         'inside-string)
        ;; Inside a paren
        ((setq start (nimrod-syntax-context 'paren ppss))
         'inside-paren)
        ;; After beginning of block
        ((setq start (save-excursion
                       (when (progn
                               (back-to-indentation)
                               (nimrod-util-forward-comment -1)
                               (looking-back nimrod-indent-indenters
                                             (line-beginning-position)))
                         ;; Move to the first block start that's not in within
                         ;; a string, comment or paren and that's not a
                         ;; continuation line.
                         (while (and (re-search-backward
                                      (nimrod-rx block-start) nil t)
                                     (nimrod-syntax-context-type)))
                         (when (looking-at (nimrod-rx block-start))
                           ;; block starts might not be at the first whitespace,
                           ;; however, we need the beginning
                           (back-to-indentation)
                           (point-marker)))))
         'after-beginning-of-block)
        ;; After normal line
        ((setq start (save-excursion
                       (back-to-indentation)
                       (skip-chars-backward (rx (or whitespace ?\n)))
                       (nimrod-nav-beginning-of-statement)
                       (point-marker)))
         'after-line)
        ;; Do not indent
        (t 'no-indent))
       start))))

(defun nimrod-indent-calculate-indentation ()
  "Calculate correct indentation offset for the current line."
  (let* ((indentation-context (nimrod-indent-context))
         (context-status (car indentation-context))
         (context-start (cdr indentation-context)))
    (save-restriction
      (widen)
      (save-excursion
        (case context-status
          ('no-indent 0)
          ;; When point is after beginning of block just add one level
          ;; of indentation relative to the context-start
          ('after-beginning-of-block
           (goto-char context-start)
           (+ (current-indentation) nimrod-indent-offset))
          ;; When after a simple line just use previous line
          ;; indentation, in the case current line starts with a
          ;; `nimrod-indent-dedenters' de-indent one level.
          ('after-line
           (-
            (save-excursion
              (goto-char context-start)
              (current-indentation))
            (if (progn
                  (back-to-indentation)
                  (looking-at nimrod-indent-dedenters))
                nimrod-indent-offset
              0)))
          ;; When inside of a string, do nothing. just use the current
          ;; indentation.  XXX: perhaps it would be a good idea to
          ;; invoke standard text indentation here
          ('inside-string
           (goto-char context-start)
           (current-indentation))
          ;; When inside a paren there's a need to handle nesting
          ;; correctly
          ('inside-paren
           (cond
            ;; If current line closes the outermost open paren use the
            ;; current indentation of the context-start line.
            ((save-excursion
               (skip-syntax-forward "\s" (line-end-position))
               (when (and (looking-at (regexp-opt '(")" "]" "}")))
                          (progn
                            (forward-char 1)
                            (not (nimrod-syntax-context 'paren))))
                 (goto-char context-start)
                 (current-indentation))))
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
                                     (nimrod-util-forward-comment))
                                   (looking-at "$")))
                           (+ (current-indentation) nimrod-indent-offset)
                         (current-column)))))
               ;; Adjustments
               (cond
                ;; If current line closes a nested open paren de-indent one
                ;; level.
                ((progn
                   (back-to-indentation)
                   (looking-at (regexp-opt '(")" "]" "}"))))
                 (- indent nimrod-indent-offset))
                ;; If the line of the opening paren that wraps the current
                ;; line starts a block add another level of indentation to
                ;; follow new pep8 recommendation. See: http://ur1.ca/5rojx
                ((save-excursion
                   (when (and content-starts-in-newline
                              (progn
                                (goto-char context-start)
                                (back-to-indentation)
                                (looking-at (nimrod-rx block-start))))
                     (+ indent nimrod-indent-offset))))
                (t indent)))))))))))

(defun nimrod-indent-calculate-levels ()
  "Calculate `nimrod-indent-levels' and reset `nimrod-indent-current-level'."
  (let* ((indentation (nimrod-indent-calculate-indentation))
         (remainder (% indentation nimrod-indent-offset))
         (steps (/ (- indentation remainder) nimrod-indent-offset)))
    (setq nimrod-indent-levels (list 0))
    (dotimes (step steps)
      (push (* nimrod-indent-offset (1+ step)) nimrod-indent-levels))
    (when (not (eq 0 remainder))
      (push (+ (* nimrod-indent-offset steps) remainder) nimrod-indent-levels))
    (setq nimrod-indent-levels (nreverse nimrod-indent-levels))
    (setq nimrod-indent-current-level (1- (length nimrod-indent-levels)))))

(defun nimrod-indent-toggle-levels ()
  "Toggle `nimrod-indent-current-level' over `nimrod-indent-levels'."
  (setq nimrod-indent-current-level (1- nimrod-indent-current-level))
  (when (< nimrod-indent-current-level 0)
    (setq nimrod-indent-current-level (1- (length nimrod-indent-levels)))))

(defun nimrod-indent-line (&optional force-toggle)
  "Internal implementation of `nimrod-indent-line-function'.
Uses the offset calculated in
`nimrod-indent-calculate-indentation' and available levels
indicated by the variable `nimrod-indent-levels' to set the
current indentation.

When the variable `last-command' is equal to one of the symbols
inside `nimrod-indent-trigger-commands' or FORCE-TOGGLE is
non-nil it cycles levels indicated in the variable
`nimrod-indent-levels' by setting the current level in the
variable `nimrod-indent-current-level'.

When the variable `last-command' is not equal to one of the
symbols inside `nimrod-indent-trigger-commands' and FORCE-TOGGLE
is nil it calculates possible indentation levels and saves it in
the variable `nimrod-indent-levels'.  Afterwards it sets the
variable `nimrod-indent-current-level' correctly so offset is
equal to (`nth' `nimrod-indent-current-level'
`nimrod-indent-levels')"
  (or
   (and (or (and (memq this-command nimrod-indent-trigger-commands)
                 (eq last-command this-command))
            force-toggle)
        (not (equal nimrod-indent-levels '(0)))
        (or (nimrod-indent-toggle-levels) t))
   (nimrod-indent-calculate-levels))
  (let* ((starting-pos (point-marker))
         (indent-ending-position
          (+ (line-beginning-position) (current-indentation)))
         (follow-indentation-p
          (or (bolp)
              (and (<= (line-beginning-position) starting-pos)
                   (>= indent-ending-position starting-pos))))
         (next-indent (nth nimrod-indent-current-level nimrod-indent-levels)))
    (unless (= next-indent (current-indentation))
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-to next-indent)
      (goto-char starting-pos))
    (and follow-indentation-p (back-to-indentation)))
  ;(nimrod-info-closing-block-message)
  )

(defun nimrod-indent-line-function ()
  "`indent-line-function' for Nimrod mode.
See `nimrod-indent-line' for details."
  (nimrod-indent-line))

(defun nimrod-indent-dedent-line ()
  "De-indent current line."
  (interactive "*")
  (when (and (not (nimrod-syntax-comment-or-string-p))
             (<= (point-marker) (save-excursion
                                  (back-to-indentation)
                                  (point-marker)))
             (> (current-column) 0))
    (nimrod-indent-line t)
    t))

(defun nimrod-indent-dedent-line-backspace (arg)
  "De-indent current line.
Argument ARG is passed to `backward-delete-char-untabify' when
point is  not in between the indentation."
  (interactive "*p")
  (when (not (nimrod-indent-dedent-line))
    (backward-delete-char-untabify arg)))
(put 'nimrod-indent-dedent-line-backspace 'delete-selection 'supersede)

(defun nimrod-indent-region (start end)
  "Indent a nimrod region automagically.

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
                         (or (not (nimrod-syntax-context 'string))
                             (eq
                              (syntax-after
                               (+ (1- (point))
                                  (current-indentation)
                                  (nimrod-syntax-count-quotes (char-after) (point))))
                              (string-to-syntax "|"))))
                (beginning-of-line)
                (delete-horizontal-space)
                (indent-to (nimrod-indent-calculate-indentation)))))
        (forward-line 1))
      (move-marker end nil))))

(defun nimrod-indent-shift-left (start end &optional count)
  "Shift lines contained in region START END by COUNT columns to the left.
COUNT defaults to `nimrod-indent-offset'.  If region isn't
active, the current line is shifted.  The shifted region includes
the lines in which START and END lie.  An error is signaled if
any lines in the region are indented less than COUNT columns."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (if count
      (setq count (prefix-numeric-value count))
    (setq count nimrod-indent-offset))
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

(defun nimrod-indent-shift-right (start end &optional count)
  "Shift lines contained in region START END by COUNT columns to the left.
COUNT defaults to `nimrod-indent-offset'.  If region isn't
active, the current line is shifted.  The shifted region includes
the lines in which START and END lie."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (let ((deactivate-mark nil))
    (if count
        (setq count (prefix-numeric-value count))
      (setq count nimrod-indent-offset))
    (indent-rigidly start end count)))

(defun nimrod-indent-electric-colon (arg)
  "Insert a colon and maybe de-indent the current line.
With numeric ARG, just insert that many colons.  With
\\[universal-argument], just insert a single colon."
  (interactive "*P")
  (self-insert-command (if (not (integerp arg)) 1 arg))
  (when (and (not arg)
             (eolp)
             (not (equal ?: (char-after (- (point-marker) 2))))
             (not (nimrod-syntax-comment-or-string-p)))
    (let ((indentation (current-indentation))
          (calculated-indentation (nimrod-indent-calculate-indentation)))
      ;(nimrod-info-closing-block-message)
      (when (> indentation calculated-indentation)
        (save-excursion
          (indent-line-to calculated-indentation)
          ;; (when (not (nimrod-info-closing-block-message))
          ;;   (indent-line-to indentation)))))))
          )))))
(put 'nimrod-indent-electric-colon 'delete-selection t)

(defun nimrod-indent-post-self-insert-function ()
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
      (when (nimrod-syntax-context 'paren)
        (let ((indentation (nimrod-indent-calculate-indentation)))
          (when (< (current-indentation) indentation)
            (indent-line-to indentation)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Utility functions ...                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nimrod-util-forward-comment (&optional direction)
  "Nimrod mode specific version of `forward-comment'.
Optional argument DIRECTION defines the direction to move to."
  (let ((comment-start (nimrod-syntax-context 'comment))
        (factor (if (< (or direction 0) 0)
                    -99999
                  99999)))
    (when comment-start
      (goto-char comment-start))
    (forward-comment factor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Navigation functions ...                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nimrod-nav-beginning-of-statement ()
  "Move to start of current statement."
  (interactive "^")
  (while (and (or (back-to-indentation) t)
              (not (bobp))
              (when (or (nimrod-syntax-context 'string)
                        (nimrod-syntax-context 'paren))
                (forward-line -1))))
  (point-marker))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Wrap it all up ...                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar nimrod-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.") 'nimrod-goto-sym)
    (define-key map (kbd "C-c h") 'nimrod-explain-sym)
    (define-key map ":" 'nimrod-indent-electric-colon)
    (define-key map "\C-c<" 'nimrod-indent-shift-left)
    (define-key map "\C-c>" 'nimrod-indent-shift-right)
    map))

(define-derived-mode nimrod-mode prog-mode
  "nimrod mode"
  "A major mode for the Nimrod programming language."

  (setq mode-name "Nimrod")  ;; This will display in the mode line.

  (set (make-local-variable 'font-lock-defaults)
       '(nimrod-font-lock-keywords nil t))

  ;; modify the keymap
  (set (make-local-variable 'indent-line-function) 'nimrod-indent-line-function)
  (set (make-local-variable 'indent-region-function) #'nimrod-indent-region)

  ;; Documentation comment highlighting
  ;; (modify-syntax-entry ?\# ". 12b" nimrod-mode-syntax-table)
  ;; (modify-syntax-entry ?\n "> b" nimrod-mode-syntax-table)

  ;; Comment
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#+\\s-*")

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
  "Path to the nimrod executable. You don't need to set this if
the nimrod executable is inside your PATH."
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
the compiled file. The callback is executed on success with the
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
    (if (bufferp "*nimrod-compile*")
        (with-current-buffer "*nimrod-compile*"
          (erase-buffer)))
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
  "Starting at current point, find the point of completion."
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
                (string-match "^\\(?:\\(.*\\)\\.\\)?\\([^.]*\\)$" fn)
                (make-nimrod-ide
                 :type type
                 :namespace (match-string 1 fn)
                 :name (match-string 2 fn)
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
