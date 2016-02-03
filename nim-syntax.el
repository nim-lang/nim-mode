;;; nim-syntax.el --- -*- lexical-binding: t -*-

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

;;; Commentary:

;;

;;; Code:
(eval-and-compile (require 'nim-rx))

(defconst nim-font-lock-keywords
  `((,(nim-rx (1+ "\t")) . 'nim-tab-face)
    (,(nim-rx defun
              (? (group (1+ " ") (or identifier quoted-chars)
                        (0+ " ") (? (group "*"))))
              (? (minimal-match
                  (group (0+ " ") "[" (0+ (or any "\n")) "]")))
              (? (minimal-match
                  (group (0+ " ") "(" (0+ (or any "\n")) ")")))
              (? (group (0+ " ") ":" (0+ " ")
                        (? (group (or "ref" "ptr") " " (* " ")))
                        (group identifier))))
     (1 (if (match-string 2)
            'nim-font-lock-export-face
          font-lock-function-name-face)
        keep t)
     (7 font-lock-type-face keep t))
    ;; Highlight type words
    (,(nim-rx (or identifier quoted-chars) (? "*")
              (* " ") ":" (* " ")
              (? (and "var " (0+ " ")))
              (? (group (and (or "ref" "ptr") " " (* " "))))
              (group identifier))
     (1 font-lock-keyword-face keep t)
     (2 (if (< 0 (nth 0 (syntax-ppss)))
            font-lock-type-face
          'default)
        keep))
    ;; This only works if it’s one line
    (,(nim-rx (or "var" "let" "const" "type") (1+ " ")
              (group (or identifier quoted-chars) (? " ") (? (group "*"))))
     . (1 (if (match-string 2)
              'nim-font-lock-export-face
            font-lock-variable-name-face)))
    ;; For multiple line properties
    (,(nim-rx line-start (1+ " ")
              (group
               (or identifier quoted-chars) "*"
               (? (and "[" word "]"))
               (0+ (and "," (? (0+ " "))
                        (or identifier quoted-chars) "*")))
              (0+ " ") (or ":" "{." "=") (0+ nonl)
              line-end)
     . (1 'nim-font-lock-export-face))
    ;; Number literal
    (,(nim-rx ; u?int
       (group int-lit)
       (? (group (? "'") (or (and (in "uUiI") (or "8" "16" "32" "64"))
                             (in "uU")))))
     (1 'nim-font-lock-number-face)
     (2 font-lock-type-face nil t))
    (,(nim-rx ; float
       (group (or float-lit dec-lit oct-lit bin-lit))
              (? (group (? "'") float-suffix)))
     (1 'nim-font-lock-number-face)
     (2 font-lock-type-face t t)  ; exponential
     (3 font-lock-type-face t t)) ; type
    (,(nim-rx ; float hex
       (group hex-lit)
       (? (group "'" float-suffix))) ; "'" isn’t optional
     (1 'nim-font-lock-number-face)
     (3 font-lock-type-face nil t))
    ;; other keywords
    (,(nim-rx (or exception type)) . font-lock-type-face)
    (,(nim-rx constant) . font-lock-constant-face)
    (,(nim-rx builtin) . font-lock-builtin-face)
    (,(nim-rx keyword) . font-lock-keyword-face)
    ;; Result
    (,(rx symbol-start "result" symbol-end) . font-lock-variable-name-face)
    ;; pragma
    (,(nim-rx pragma) . (0 'nim-font-lock-pragma-face keep)))
  "Font lock expressions for Nim mode.")

(defsubst nim-syntax-count-quotes (quote-char &optional point limit)
  "Count number of quotes around point (max is 3).
QUOTE-CHAR is the quote char to count.  Optional argument POINT is
the point where scan starts (defaults to current point), and LIMIT
is used to limit the scan."
  (let ((i 0))
    (while (and (< i 3)
                (or (not limit) (< (+ point i) limit))
                (eq (char-after (+ point i)) quote-char))
      (setq i (1+ i)))
    i))

(defconst nim-syntax-propertize-function
  (syntax-propertize-rules
   ;; Char
   ;; Put syntax entry("\"") for character type to highlight
   ;; when only the character-delimiter regex matched.
   ((nim-rx character-delimiter)
    (1 "\"")  ; opening quote
    (2 "\"")) ; closing quote
   ;; String
   ((nim-rx string-delimiter)
    (0 (ignore (nim-syntax-stringify))))
   ;; multi line comment
   ((rx (or (group (or line-start (not (any "]" "#")))
                   (group "#" (? "#") "["))
            (group "]" "#" (? "#"))
            (group "#")))
    (0 (ignore (nim-syntax-commentify))))))

(defun nim-syntax-stringify ()
  "Put `syntax-table' property correctly on single/triple double quotes."
  (let* ((num-quotes (length (match-string-no-properties 1)))
         (ppss (prog2
                   (backward-char num-quotes)
                   (syntax-ppss)
                 (forward-char num-quotes)))
         (string-start (and (not (nth 4 ppss)) (nth 8 ppss)))
         (quote-starting-pos (- (point) num-quotes))
         (quote-ending-pos (point))
         (num-closing-quotes
          (and string-start
               (nim-syntax-count-quotes
                (char-before) string-start quote-starting-pos))))
    (cond ((and string-start (= num-closing-quotes 0))
           ;; This set of quotes doesn't match the string starting
           ;; kind. Do nothing.
           nil)
          ((not string-start)
           ;; This set of quotes delimit the start of a string.
           (put-text-property quote-starting-pos (1+ quote-starting-pos)
                              'syntax-table (string-to-syntax "|")))
          ((and string-start (< string-start (point))
                ;; Skip "" in the raw string literal
                (eq ?r (char-before string-start))
                (or
                 ;;  v point is here
                 ;; ""
                 (and
                  (eq ?\" (char-before (1- (point))))
                  (eq ?\" (char-before (point))))
                 ;; v point is here
                 ;; ""
                 (and
                  (eq ?\" (char-before (point)))
                  (eq ?\" (char-after  (point))))))
           nil)
          ((= num-quotes num-closing-quotes)
           ;; This set of quotes delimit the end of a string.
           ;; If there are some double quotes after quote-ending-pos,
           ;; shift the point to right number of `extra-quotes' times.
           (let* ((extra-quotes 0))
             ;; Only count extra quotes when the double quotes is 3 to prevent
             ;; wrong highlight for r"foo""bar" forms.
             (when (eq num-quotes 3)
               (while (eq ?\" (char-after (+ quote-ending-pos extra-quotes)))
                 (setq extra-quotes (1+ extra-quotes))))
             (put-text-property (+ (1- quote-ending-pos) extra-quotes)
                                (+ quote-ending-pos      extra-quotes)
                                'syntax-table (string-to-syntax "|"))))
          ((> num-quotes num-closing-quotes)
           ;; This may only happen whenever a triple quote is closing
           ;; a single quoted string. Add string delimiter syntax to
           ;; all three quotes.
           (put-text-property quote-starting-pos quote-ending-pos
                              'syntax-table (string-to-syntax "|"))))))

(defun nim-syntax-commentify ()
  (let* ((hash (or (match-string-no-properties 2)
                   (match-string-no-properties 3)
                   (match-string-no-properties 4)))
         (start-pos (- (point) (length hash)))
         (ppss (save-excursion (syntax-ppss)))
         (start-len (save-excursion
                      (when (nth 8 ppss)
                        (goto-char (nth 8 ppss))
                        (looking-at "##?\\[")
                        (length (match-string 0))))))
    (cond
     ((and (eq nil (nth 4 ppss)) (eq 1 (length hash)))
      (put-text-property start-pos (1+ start-pos)
                         'syntax-table (string-to-syntax "<"))
      (put-text-property (point-at-eol) (1+ (point-at-eol))
                         'syntax-table (string-to-syntax ">")))
     ;; ignore
     ((or (eq t (nth 4 ppss)) ; t means single line comment
          (<= (length hash) 1)
          ;; don't put syntax comment start or end
          ;; if it’s "#[" or "]#" inside ##[]##
          (and start-len (= 3 start-len) (= 2 (length hash))))
      nil)
     ;; multi comment line start
     ((eq ?# (string-to-char hash))
      (put-text-property start-pos (1+ start-pos)
                         'syntax-table (string-to-syntax "< bn")))
     ;; multi comment line end
     ((eq ?\] (string-to-char hash))
      (put-text-property (1- (point)) (point)
                         'syntax-table (string-to-syntax "> bn"))))))

(defun nim-syntax-context-type (&optional syntax-ppss)
  "Return the context type using SYNTAX-PPSS.
The type returned can be `comment', `string' or `paren'."
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (cond
     ((nth 8 ppss) (if (nth 4 ppss) 'comment 'string))
     ((nth 1 ppss) 'paren))))

(defun nim-syntax--context-compiler-macro (form type &optional syntax-ppss)
  (pcase type
    (`'comment
     `(let ((ppss (or ,syntax-ppss (syntax-ppss))))
        (and (nth 4 ppss) (nth 8 ppss))))
    (`'string
     `(let ((ppss (or ,syntax-ppss (syntax-ppss))))
        (and (nth 3 ppss) (nth 8 ppss))))
    (`'paren
     `(nth 1 (or ,syntax-ppss (syntax-ppss))))
    (_ form)))

(defun nim-syntax-context (type &optional syntax-ppss)
  "Return non-nil if point is on TYPE using SYNTAX-PPSS.
TYPE can be `comment', `string' or `paren'.  It returns the start
character address of the specified TYPE."
  (declare (compiler-macro nim-syntax--context-compiler-macro))
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (pcase type
      (`comment (and (nth 4 ppss) (nth 8 ppss)))
      (`string (and (nth 3 ppss) (nth 8 ppss)))
      (`paren (nth 1 ppss))
      (_ nil))))

(defsubst nim-syntax-comment-or-string-p (&optional ppss)
  "Return non-nil if PPSS is inside 'comment or 'string."
  (nth 8 (or ppss (syntax-ppss))))

(defsubst nim-syntax-closing-paren-p ()
  "Return non-nil if char after point is a closing paren."
  (= (syntax-class (syntax-after (point)))
     (syntax-class (string-to-syntax ")"))))

(provide 'nim-syntax)
;;; nim-syntax.el ends here

