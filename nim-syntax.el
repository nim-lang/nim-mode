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
    (,(nim-rx defun (1+ " ")
              (group (or identifier quoted-chars)
                     (0+ " ") (? (group "*"))))
     . (1 (if (match-string 2)
              'nim-font-lock-export-face
            font-lock-function-name-face)))
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
    (,(nim-rx (or exception type)) . font-lock-type-face)
    (,(nim-rx constant) . font-lock-constant-face)
    (,(nim-rx builtin) . font-lock-builtin-face)
    (,(nim-rx keyword) . font-lock-keyword-face)
    (,(nim-rx "{." (1+ any) ".}") . font-lock-preprocessor-face)
    (,(nim-rx (or identifier quoted-chars) (? "*")
              (* " ") ":" (* " ") (group identifier))
     . (1 font-lock-type-face)))
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

;; from python?
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
    (0 (ignore (nim-syntax-stringify))))))

;; python?
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
          ((= num-quotes num-closing-quotes)
           ;; This set of quotes delimit the end of a string.
           ;; If there are some double quotes after quote-ending-pos,
           ;; shift the point to right number of `extra-quotes' times.
           (let* ((extra-quotes 0))
             ;; Only count extra quotes when the double quotes is 3 to prevent
             ;; wrong highlight for r"foo""bar" forms.
             (when (eq num-quotes 3)
               (while (eq 34 (char-after (+ quote-ending-pos extra-quotes)))
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

