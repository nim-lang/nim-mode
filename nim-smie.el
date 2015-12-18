;;; nim-smie.el --- Nim’s indent support library using smie.el -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Yuta Yamada
;; Author: Yuta Yamada

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
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Configuration:
;;   To activate smie for nim-mode set below configuration before
;;   loading nim-mode.
;;
;;   (setq nim-use-smie-indent t)
;;
;; TODO: remove copied if-let in nim-helper.el (after 25 is majored)
;; TODO: electric-colon
;; TODO: cycle indent
;;; Code:

(require 'subr-x nil t) ; for if-let (from Emacs 25.1)
(require 'nim-helper)
(require 'smie)   ; Emacs’ indentation library

;; INTERNAL VARIABLES
(defvar nim-smie-indent-start-point nil)
(defvar nim-smie-previous-line-indent-state nil)
(defvar nim-smie-force-indent-column nil)
(defvar nim-smie-after-indent-hook nil)

(defconst nim-mode-smie-grammar
  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2
     '((id)
       (stmts (stmt ";" stmt) (stmts))
       (stmt (exp) (exp) (exp "break"))
       (exp (id) (exp)  (virtual-indents))
       ;; To highlight between & themselves
       (str (exp "&" exp "__dedenter") (str))
       (decl
        ("var" exp) ("let" exp) ("const" exp) ("type" exp) ("import" exp)
        ;; just for single line var and let
        ("__single_line")
        ;; Object
        ("object" stmts "__obj_end")
        ("object" "of" stmts "__obj_end")
        ;; Enum
        ("enum" stmts "__enum_end")
        ("tuple" stmts "__tuple_end"))
       (functions
        ("proc" exp "=") ("template" exp "=") ("macro" exp "=")
        ("iterator" exp "=") ("converter" exp "=") (exp "=" exp))
       (conditions
        ("if" exp "elif" exp "else" ":")
        ("when" exp "elif" exp "else" ":")
        ("case" exp "of" exp "else" ":")
        ("case" exp "of" exp "elif" exp "else" ":")
        ("case" exp "elif" exp "else" ":")
        ("try" exp "except" exp "except" exp "finally" ":"))
       (br
        ("while" conditions "break")
        ("for" exp "in" conditions "break")
        ("block" conditions "break"))
       (virtual-indents
        (stmts "__indent_stopper") (stmts "__dedenter")
        (stmts "__after_break")))
     ;; You can choose: `assoc', `left', `right' or `nonassoc'.
     '((nonassoc "if" "when" "case" "for" "try")
       (assoc "of") (assoc "elif") (assoc "else"))
     '((assoc "case") (assoc "else") (assoc ":"))
     '((nonassoc "case" "object") (assoc "of"))
     '((assoc "for") (assoc "in") (assoc ":"))
     '((assoc "try") (assoc "except") (assoc "finally") (assoc  ":"))
     ;; Functions
     '((assoc "proc" "template" "macro" "iterator" "converter") (assoc "="))
     ;; While
     '((nonassoc "while" "block" "for") (assoc "break"))
     '((assoc "=") (nonassoc "block" "while"))
     ;; operators from nim manual
     '((assoc "$" "^") (assoc "*" "%" "\\" "/" "div" "mod" "shl" "shr")
       (assoc "+" "-" "~" "|"
              ;; Not sure this place is ok...
              "+%" "-%" "*%" "/%" "%%" "<%" "<=%")
       (right "&") (assoc "." "..")
       (assoc "=" "<" ">" "!"
              "==" "<=" "<" "!=" "in" "notin" "is" "isnot" "not" "of")
       (assoc "and") (assoc "or" "xor") (assoc "@" ":" "?")
       (assoc "+=" "*=") ("->" "=>"))
     '((right "&") (assoc "__dedenter"))
     '((assoc "=" ";") (assoc "__indent_stopper" "__dedenter" "__after_break"))))))

(defvar nim-smie-indent-stoppers
  '("proc" "template" "macro" "iterator" "converter" "type"))

(defvar nim-smie-indent-dedenters
  '("var" "let" "const" "import" "if" "when" "case"
    "of" "elif" "else" "while" "for" "try" "block"))

(defun nim-mode-smie-rules (kind token)
  "Nim-mode’s indent rules.
See also ‘smie-rules-function’ about KIND and TOKEN."
  (pcase (cons kind token)
    ;; Paren
    (`(:after . ,(or "(" "{" "["))
     (cons 'column (+ (current-indentation) nim-indent-offset)))
    (`(:close-all . ,(or "]" "}" ")")) t)
    (`(:before . ,(or "{" "[" "("))
     (if-let ((name (smie-rule-parent-p
                     "proc" "template" "macro" "iterator" "converter")))
         (if-let ((pos (nim-helper-line-contain-p '(?\} ?\) ?\]) nil t)))
             ;; for proc4.nim of test
             (save-excursion
               (let ((bol (point-at-bol))
                     (opener (nth 1 (syntax-ppss pos))))
                 (when (< opener bol)
                   (goto-char opener)
                   (cons 'column (current-indentation)))))
           nim-smie-function-indent)
       (cons 'column (current-indentation))))
    ;; "="
    (`(,(or :before :after) . "=")
     (if-let ((parent (smie-rule-parent nim-indent-offset)))
         parent
       nim-indent-offset))
    ;; When it’s non-nil, it organizes condition parts.
    ;; ex:
    ;;      if ...long multiple conditions...
    ;;         ...long multiple conditions...:
    ;;        ↑ indent like this
    (`(:list-intro . ,(or "if" "when" "while" "elif" "block")) t)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Work around for "object of"
    (`(:list-intro  . ,(or "of")) nil) ; to call elem args
    (`(:elem . args)
     (let ((tk (funcall smie-backward-token-function)))
       (if (stringp tk)
           (nim-mode-smie-rules :after tk)
         0)))
    (`(:after  . "of")
     (when (smie-rule-prev-p "object") ; this is called from :elem args
       (back-to-indentation)
       nim-indent-offset)) ; need to be offset value
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; For case statement
    (`(:before . "of") (when (smie-rule-prev-p ":") nim-indent-offset))
    (`(:before . "else") (nim-smie-rule-adjust-else-stmt))
    ;; Use SMIE’s default configuration
    ;; (nil means defualt)
    (`(:before . ,(or ";" "," "`"))
     nil)
    ;; Indent after those declarations
    (`(:after . ,(or "var" "let" "const" "type" "import"))
     nim-indent-offset)
    ;; COLON
    (`(,(or :before :after) . ":")
     (cl-case kind
       (:after
        ;; General ":" rule
        (if (not (smie-rule-parent-p "if" "when" "case" "while" "of" "elif"
                                     "for" "try" "except" "finally"))
            (cons 'column (+ (current-indentation) nim-indent-offset))
          ;; else after case statement
          (if (and (smie-rule-prev-p "else")
                   (smie-rule-parent-p "case"))
              (cons 'column (+ (cdr (nim-smie-rule-adjust-else-stmt))
                               nim-indent-offset))
            ;; other conditions
            (if-let ((parent (smie-rule-parent nim-indent-offset)))
                parent))))
       (:before
        ;; Indent after ":" for Nim’s control statements and macros
        (if (and (smie-rule-prev-p "else")
                 (smie-rule-parent-p "case"))
            (cons 'column (+ (cdr (nim-smie-rule-adjust-else-stmt))
                             nim-indent-offset))
          nim-indent-offset))))
    ;; &
    (`(:after . "&")
     (save-excursion
       (condition-case _err
           (if (smie-rule-sibling-p)
               (cons 'column (current-indentation))
             (cons 'column (+ (current-indentation) nim-indent-offset)))
         (error
          ;; list-intro & -> :elem args -> here
          (- (+ (current-indentation) (* nim-indent-offset 2)))))))
    (`(:list-intro . "&") nil)
    (`(:before . ,(or "enum" "object" "tuple"))
     (save-excursion (back-to-indentation)
                     (cons 'column (+ (current-column) nim-indent-offset))))
    ;; Below keywords might be top level, so for convenience,
    ;; if previous line is an empty line or something similar,
    ;; set indent to 0.
    (`(:before . "__indent_stopper") (cons 'column 0))
    ;; dedent if previous line is empty and TOKEN is one of
    ;; ‘nim-smie-indent-dedenters’.
    (`(:before . "__dedenter") (smie-rule-parent))
    ;; break
    (`(:before . "break") (nim-smie-rule-before-break))
    ;; dedent after break. Note that this token (break -> __dedenter)
    ;; conversion is needed following case:
    ;;
    ;;   while true:
    ;;     echo "process"
    ;;     if true:
    ;;       echo "something"
    ;;       break
    ;;     echo "<- after break should be dedented"
    ;;
    ;; If you use "break" instead, it destructs if/elif/else’s
    ;; indent logic after break.
    (`(:after  . "__after_break")
     (nim-set-force-indent (- (current-indentation) nim-indent-offset)))
    ;; other keywords
    (`(:before . ,_))
    ;; Don’t make ambiguous indentation
    (_ 0)))

(defun nim-set-force-indent (indent)
  (setq nim-smie-force-indent-column indent)
  nil)

(defun nim-smie-force-indent ()
  "Overall SMIE’s indentation."
  (when nim-smie-force-indent-column
    (goto-char nim-smie-indent-start-point)
    (indent-line-to nim-smie-force-indent-column)))
(add-hook 'nim-smie-after-indent-hook 'nim-smie-force-indent)

(defun nim-line-empty-p (&optional line)
  "Return non-nil if current line is empty, ignoring whitespace.
If there is the optional LINE argument, moves LINE times from current line."
  (catch 'failed
    (save-excursion
      (when line
        (when (not (line-move line t))
          (throw 'failed nil)))
      (beginning-of-line 1)
      (looking-at
       (nim-rx line-start (* whitespace)
               (group (* not-newline))
               (* whitespace) line-end))
      (string-equal "" (match-string-no-properties 1)))))

(defun nim-line-comment-p (&optional line start-point)
  "Return non-nil if current line's start position is comment.
If there is the optional LINE argument, moves LINE times from current line."
  (catch 'failed
    (save-excursion
      (when (and line (not (line-move line t)))
        (throw 'failed nil))
      (if (not start-point)
          (eq ?# (char-after (+ (line-beginning-position) (current-indentation))))
        (let ((ppss (syntax-ppss start-point)))
          (when (eq t (nth 4 ppss))
            (goto-char (nth 8 ppss))
            (current-column)))))))

;; Some users might want to avoid this?
(defun nim-smie-previous-line-predicate ()
  "Return non-nil if previous line is an empty line.
Or an empty line after comment line(s)"
  (save-excursion
    (goto-char nim-smie-indent-start-point)
    (if (nim-line-empty-p -1)
        t
      (when (nim-line-comment-p -1 )
        (catch 'found
          (while (and (line-move -1 t) (not (nim-line-comment-p)))
            (when (nim-line-empty-p -1)
              (throw 'found t))))))))

(defun nim-mode-forward-token ()
  (let ((_pos (point)))
    (skip-chars-forward " \t")
    (forward-comment (point-max))
    (let* ((tok (smie-default-forward-token)))
      (setq tok (nim-smie-convert-token tok))
      (when (nim-smie-previous-line-predicate)
        (cond ((member tok nim-smie-indent-stoppers)
               (setq tok "__indent_stopper"))
              ((member tok nim-smie-indent-dedenters)
               (setq tok "__dedenter"))))
      tok)))

(defun nim-mode-backward-token ()
  (let ((pos (point)))
    (forward-comment (- pos))
    (skip-chars-backward " \t")
    (let* ((tok (nim-smie-convert-token (smie-default-backward-token))))
      (if (eq 'empty nim-smie-previous-line-indent-state)
          (progn (goto-char (1- pos))
            (setq tok "\n"))
        (cond
         ;; TODO: make better handling
         ((equal "break" tok)
          (setq tok "__after_break"))
         ((member tok '("proc" "template" "macro" "iterator" "converter"))
          ;; check current token is not return type of function signature.
          ;; if so, avoid the token.
          (unless (eq (point) (+ (point-at-bol) (current-indentation)))
            (save-excursion
              (goto-char (+ (point-at-bol)
                            (current-indentation)))
              (when (member (thing-at-point 'symbol)
                            '("proc" "template" "macro" "iterator" "converter"))
                (setq tok ".")))))
         ((equal "." tok)
          (setq tok ""))
         ((member tok '(":" "="))
          (if (and (not (member (char-before (point-at-eol)) '(?: ?=)))
                   (not (looking-at "[=:] +#.+$")))
              ;; avoid infix: ":", "=".
              (setq tok "")))))
      tok)))

(defun nim-smie-convert-token (token)
  "Convert TOKEN to another."
  (cond
   ((member token '("var" "let" "const" "import" "type"))
    (if (looking-at ; FIXME: use more precise regex
         (rx (or "var" "let" "const" "import" "type") (1+ " ") (or (not (any "#")) line-end)))
        "__single_line"
      token))
   (t token)))

(defun nim-smie-rule-adjust-else-stmt ()
  "If case statement ends with colon, it should be indented."
  (when (smie-rule-parent-p "case")
    (save-excursion
      (let ((parent (smie-indent--parent)))
        (cond
         ((and (equal "case" (nth 2 parent))
               ;; Don’t search if the "case" is single line stmt.
               (< (nth 1 parent) (point-at-bol)))
          (goto-char (nth 1 parent))
          (let* (target-token)
            (while (and (not (member (car target-token) '("of" "else")))
                        (not (eobp)))
              (setq target-token (smie-indent-forward-token)))
            (when (equal "of" (car target-token))
              (cons 'column (- (current-column) 2))))))))))

(defun nim-smie-rule-before-break ()
  "Calculate indentation of :before break."
  (if-let ((parent (smie-rule-parent-p "while" "block" "for")))
      (smie-rule-parent
       (cond
        ((smie-rule-prev-p ":")
         ;; For:
         ;;   for x in foo:
         ;;     if true: break
         ;;     echo "close if's indent"
         (let ((bol (point-at-bol)))
           (save-excursion
             (nim-mode-backward-token)
             (if (< (point) bol)
                 (* nim-indent-offset 2)
               ;; TODO: make sure this case
               nim-indent-offset))))
        (t
         ;; For:
         ;;   for x in foo:
         ;;     ...
         ;;     if condition:
         ;;       xxx
         ;;       break <- indent until if’s scope
         (save-excursion
           (goto-char (1- (point-at-bol)))
           (let ((previous-line (current-indentation))
                 (parent-indent
                  (progn (goto-char (nth 1 (smie-indent--parent)))
                         (current-column))))
             (- previous-line parent-indent))))))
    (smie-rule-parent nim-indent-offset)))

(defun nim-previous-line-end-with (char)
  (save-excursion
    (when (line-move -1 t)
      (eq char (char-before (point-at-eol))))))

(defun nim-get-comment-indent ()
  "Return indent number for comment.
This works if only current line starts from comment."
  (when (nim-line-comment-p)
    (if-let ((column (and (not (eq (point-min) (point-at-bol)))
                          (nim-line-comment-p nil (- (point-at-bol) 2)))))
        column
      ;; docgen comment
      (when (and
             (not (nim-line-comment-p -1))
             (eq ?# (char-after (+ 1 (point-at-bol) (current-indentation)))))
        (save-excursion
          (when (line-move -1 t)
            (+ (current-indentation) nim-indent-offset)))))))

(defun nim-get-comment-start-point ()
  "Return comment starting point."
  (if-let ((ppss (and
                  (not (eq (point-min) (point-at-bol)))
                  (save-excursion (syntax-ppss (- (point-at-bol) 2))))))
      (when (eq t (nth 4 ppss))
        (nth 8 ppss))))

(defun nim-smie-indent-line-function ()
  "Indent function using smie library."
  (interactive)
  (setq nim-smie-indent-start-point (point)
        nim-smie-previous-line-indent-state nil
        nim-smie-force-indent-column nil)
  (if-let ((comment-indent (nim-get-comment-indent)))
      ;; indent for comment
      (indent-line-to comment-indent)
    (smie-indent-line)
    (run-hooks 'nim-smie-after-indent-hook)
    ;; when ‘smie-indent-lien’ returns ’noindent, somehow
    ;; it indents. To work around, return t.
    ;; (you can check at string.nim in SMIE directory and it couldn’t
    ;; check at my test)
    t))

(provide 'nim-smie)
;;; nim-smie.el ends here
