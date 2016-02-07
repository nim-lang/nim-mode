;;; test-syntax.el --- Tests for syntax.el -*-lexical-binding:t-*-

;;; Code:

(require 'nim-mode)
(require 'cl-lib)

(describe
 "Syntax"
 (before-each
  (set-buffer (get-buffer-create "*Test*"))
  (erase-buffer)
  (nim-mode))

 (after-each
  (kill-buffer (get-buffer-create "*Test*")))

 (defun test-concat-dir (filepath)
   (if noninteractive
       filepath
     (defvar-local test-syntax-dir (concat (locate-dominating-file buffer-file-name ".git")))
     (concat test-syntax-dir filepath)))

 (defun test-faces (test-string file-name faces)
   (it test-string
       (insert-file-contents-literally file-name)
       (font-lock-default-fontify-buffer)
       (dolist (pos-face faces)
         (expect
          (get-text-property (car pos-face) 'face)
          :to-equal
          (cdr pos-face)))))

 (defun test-faces-by-range (test-string file-name spec &optional not)
   (it test-string
       (insert-file-contents-literally file-name)
       (font-lock-default-fontify-buffer)
       (cl-loop for (place . expected-face) in spec
                for start = (car place)
                for end   = (cdr place)
                do (test-helper-range-expect start end expected-face not))))

 (defun test-helper-range-expect (start end face &optional not)
   "Expect FACE between START and END positions."
   (cl-loop for pos from start to end
            do (expect
                (get-text-property pos 'face)
                (when not :not)
                :to-equal
                face)))

 (defun test-characters (test-string file-name)
   (it test-string
       (insert-file-contents-literally file-name)
       (font-lock-default-fontify-buffer)
       (goto-char (point-min))
       (search-forward "testCharacters: set[char] = {\n    ")
       (let* ((limit (+ 128 22 128))    ; to prevent eternal loop
              char-points
              after-char-points
              checked-characters)
         (catch 'exit
           (while (and (not (eql 0 limit)))
             (setq limit (1- limit))
             (re-search-forward
              (rx (group "'" (regex "[^']\\{1,4\\}") "'")
                  (group (or (1+ "," (or blank "\n"))
                             (and "\n" (* blank) "}")))) nil t)
             (let ((char       (match-string 1))
                   (after-char (match-string 2)))
               (when char
                 (let* ((start (- (point) (+ (length char) (length after-char))))
                        (end (+ start (1- (length char)))))
                   (push (cons start end) char-points)
                   (push (substring-no-properties char) checked-characters)))
               (when after-char
                 (let* ((start2 (- (point) (length after-char)))
                        (end2 (1- (point))))
                   (push (cons start2 end2) after-char-points)))
               (when (string-match "\\XFF" char)
                 (throw 'exit nil)))))
         (when char-points
           (cl-loop for (s . e) in char-points
                    do (test-helper-range-expect s e 'font-lock-string-face)))
         (when after-char-points
           (cl-loop for (s . e) in after-char-points
                    do (test-helper-range-expect s e 'font-lock-string-face t)))
         ;; You can check what you checked
         ;; (print (reverse checked-characters))
         )))

 (defun test-double-quote-and-next-line (test-string file-name start-strings)
   (let ((file-name file-name)
         (start-strings start-strings)
         (check-highlight
          (lambda (string)
            (goto-char (point-min))
            (if (search-forward string nil t)
                (test-helper-range-expect (point) (1- (point-at-eol)) 'font-lock-string-face)
              (error (format "Failed to find start string: %s" string)))
            (when (line-move 1 t)
              ;; comment line should not be highlighted by 'font-lock-string-face
              (test-helper-range-expect (1+ (point-at-bol)) (1- (point-at-eol)) 'font-lock-comment-face)))))
     (it test-string
         (insert-file-contents-literally file-name)
         (font-lock-default-fontify-buffer)
         (cl-loop for string in start-strings
                  do (funcall check-highlight string)))))

 ;; String
 (test-faces
  ;; You can check which faces are at a position with
  ;; (text-properties-at pos (get-buffer "file.nim"))
  "should highlight strings"
  (test-concat-dir "tests/syntax/string.nim")
  '((10 . font-lock-string-face)
    (5 . font-lock-variable-name-face)))

 (test-faces-by-range
  "should highlight inside here document"
  (test-concat-dir "tests/syntax/string.nim")
  '(((33 . 86) . font-lock-string-face)))

 (test-faces-by-range
  "should highlight export variables"
  (test-concat-dir "tests/syntax/export.nim")
  '(((5   . 16)  . nim-font-lock-export-face)
    ((40  . 50)  . nim-font-lock-export-face)
    ((69  . 80)  . nim-font-lock-export-face)
    ((103 . 106) . nim-font-lock-export-face)))

 (test-faces-by-range
  "should highlight function name"
  (test-concat-dir "tests/syntax/function_name.nim")
  '(((6   . 8)  . font-lock-function-name-face)))

 (test-double-quote-and-next-line
  "should highlight double quoted string with single quotes"
  ;; This test makes sure whether highlights after "xxx = " are
  ;; font-lock-string-face and next lines are not affected by the string
  ;; face.
  (test-concat-dir "tests/syntax/string.nim")
  '("var endOfQuote = "         "var endOfQuote2 = "
    "var beginningOfQuote = "   "var beginningOfQuote2 = "
    "var enclosedQuote = "      "var enclosedQuote2 = "
    "var escapedSingleQuote = " "var escapedSingleQuote2 = "
    "var escapedDoubleQuote = " "var escapedDoubleQuote2 = "
    "var unbalancedDoubleQuote = " "var unbalancedDoubleQuote2 = "
    "var unbalancedDoubleQuote3 = " "var unbalancedDoubleQuote4 = "
    "var rawString = r"
    ))

 ;; Character
 (test-characters
  "should highlight characters correctly"
  (test-concat-dir "tests/syntax/char.nim"))

 ;; Number
 (test-faces-by-range
  "should highlight numbers"
  (test-concat-dir "tests/syntax/number.nim")
  '(((21   . 23)   . nim-font-lock-number-face);uint
    ((24   . 25)   . font-lock-type-face)      ;uint type
    ((43   . 43)   . nim-font-lock-number-face);i8
    ((44   . 46)   . font-lock-type-face)      ;i8 type
    ((64   . 65)   . nim-font-lock-number-face);i16
    ((66   . 69)   . font-lock-type-face)      ;i16 type
    ((87   . 88)   . nim-font-lock-number-face);i32
    ((89   . 92)   . font-lock-type-face)      ;i32 type
    ((110  . 111)  . nim-font-lock-number-face);i64
    ((112  . 115)  . font-lock-type-face)      ;i64 type
    ((133  . 133)  . nim-font-lock-number-face);f32
    ((137  . 140)  . font-lock-type-face)      ;f32 type
    ((158  . 158)  . nim-font-lock-number-face);f64
    ((162  . 165)  . font-lock-type-face)))    ;f64 type

 ;; Pragma
 (test-faces-by-range
  "should highlight pragmas"
  (test-concat-dir "tests/syntax/pragma.nim")
  '(((31 . 41)  . nim-font-lock-pragma-face)
    ((79 . 86)  . nim-font-lock-pragma-face)))

 ;; docstring
 (test-faces-by-range
  "should highlight docstring"
  (test-concat-dir "tests/syntax/docstring.nim")
  '(((56 . 100)  . font-lock-doc-face)
    ((128 . 176) . font-lock-doc-face)))

 ;; multi line comment or doc comment
 (test-faces-by-range
  "should highlight multi line comment and doc string"
  (test-concat-dir "tests/syntax/multiline_comment.nim")
  '(((1 . 5) . font-lock-comment-delimiter-face)
    ((48 . 75)  . font-lock-string-face)
    ((77 . 78) . font-lock-comment-delimiter-face)
    ((80 . 185)  . font-lock-comment-face)
    ((192 . 235)  . font-lock-string-face)
    ((237 . 453)  . font-lock-doc-face)
    ((482 . 536)  . font-lock-doc-face)
    ((545 . 553)  . font-lock-string-face)
    ((564 . 566)  . font-lock-comment-face)
    ((567 . 576)  . font-lock-comment-delimiter-face)
    ((611 . 614)  . font-lock-string-face)
    ((615 . 620)  . font-lock-comment-face)
    ((626 . 629)  . font-lock-string-face)
    ((630 . 635)  . font-lock-comment-face)
    ((641 . 645)  . font-lock-string-face)
    ((646 . 651)  . font-lock-comment-face)
    ((657 . 661)  . font-lock-string-face)
    ((662 . 667)  . font-lock-comment-face)
    ((673 . 682)  . font-lock-string-face)
    ((683 . 701)  . font-lock-comment-face)))

 ;; varargs inside proc
 (test-faces-by-range
  "should highlight varargs inside proc’s args correctly"
  (test-concat-dir "tests/syntax/varargs.nim")
  '(((55 . 61) . font-lock-type-face)))


 ) ; end of describe function

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; test-syntax.el ends here
