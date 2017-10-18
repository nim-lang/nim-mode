;;; test-syntax.el --- Tests for syntax.el -*-lexical-binding:t-*-

;;; Code:

(load (concat default-directory "tests/helper/test-syntax-helper.el"))

(describe
 "Testing Syntax Highlight"

 (before-each
  (set-buffer (get-buffer-create "*Test*"))
  (erase-buffer)
  ;; may need to turn off this to adapt triple double quotes.
  (prettify-symbols-mode 0)
  (nim-mode))

 (after-each
  (kill-buffer (get-buffer-create "*Test*")))

 (it "should highlight with strings"
     (expect (face-at-point-of 10 :on "string.nim")
             :to-be font-lock-string-face)
     (expect (face-at-point-of 5  :on "string.nim")
             :to-be font-lock-variable-name-face))

 (it "should highlight inside here document"
     (assert-highlight-between 33 86 :on "string.nim"
                               :match font-lock-string-face))

 (it "should highlight export variables"
     (assert-highlights-between
      '((5   . 16) (40  . 50) (69  . 80) (103 . 106))
      :on "export.nim" :match 'nim-font-lock-export-face))

 (it "should highlight function name"
     (assert-highlight-between 6 8 :on "function_name.nim"
                               :match font-lock-function-name-face))

 (it "should highlight double quoted string with single quotes"
     ;; This test makes sure whether highlights after "xxx = " are
     ;; font-lock-string-face and next lines are not affected by the string
     ;; face.
     (expect-string
      :on "string.nim"
      :search-strings
      '("var endOfQuote = "         "var endOfQuote2 = "
        "var beginningOfQuote = "   "var beginningOfQuote2 = "
        "var enclosedQuote = "      "var enclosedQuote2 = "
        "var escapedSingleQuote = " "var escapedSingleQuote2 = "
        "var escapedDoubleQuote = " "var escapedDoubleQuote2 = "
        "var unbalancedDoubleQuote = " "var unbalancedDoubleQuote2 = "
        "var unbalancedDoubleQuote3 = " "var unbalancedDoubleQuote4 = "
        "var rawString = r")))

 ;; ;; Character
 ;; (test-characters
 ;;  "should highlight characters correctly"
 ;;  (test-concat-dir "tests/syntax/char.nim"))

;;  ;; Number
;;  (test-faces-by-range
;;   "should highlight numbers"
;;   (test-concat-dir "tests/syntax/number.nim")
;;   '(((21   . 23)   . nim-font-lock-number-face);uint
;;     ((24   . 25)   . font-lock-type-face)      ;uint type
;;     ((43   . 43)   . nim-font-lock-number-face);i8
;;     ((44   . 46)   . font-lock-type-face)      ;i8 type
;;     ((64   . 65)   . nim-font-lock-number-face);i16
;;     ((66   . 69)   . font-lock-type-face)      ;i16 type
;;     ((87   . 88)   . nim-font-lock-number-face);i32
;;     ((89   . 92)   . font-lock-type-face)      ;i32 type
;;     ((110  . 111)  . nim-font-lock-number-face);i64
;;     ((112  . 115)  . font-lock-type-face)      ;i64 type
;;     ((133  . 133)  . nim-font-lock-number-face);f32
;;     ((137  . 140)  . font-lock-type-face)      ;f32 type
;;     ((158  . 158)  . nim-font-lock-number-face);f64
;;     ((162  . 165)  . font-lock-type-face)))    ;f64 type

;;  ;; Pragma
;;  (test-faces-by-range
;;   "should highlight pragmas"
;;   (test-concat-dir "tests/syntax/pragma.nim")
;;   '(((31 . 40)  . nim-font-lock-pragma-face)
;;     ((79 . 85)  . nim-font-lock-pragma-face)))

;;  ;; docstring
;;  (test-faces-by-range
;;   "should highlight docstring"
;;   (test-concat-dir "tests/syntax/docstring.nim")
;;   '(((56 . 99)  . font-lock-doc-face)
;;     ((128 . 175) . font-lock-doc-face)))

;;  ;; multi line comment or doc comment
;;  (test-faces-by-range
;;   "should highlight multi line comment and doc string"
;;   (test-concat-dir "tests/syntax/multiline_comment.nim")
;;   '(((1 . 5) . font-lock-comment-face)
;;     ((48 . 75)  . font-lock-string-face)
;;     ((77 . 78) . font-lock-comment-delimiter-face)
;;     ((80 . 185)  . font-lock-comment-face)
;;     ((192 . 235)  . font-lock-string-face)
;;     ((237 . 453)  . font-lock-doc-face)
;;     ((482 . 536)  . font-lock-doc-face)
;;     ((545 . 553)  . font-lock-string-face)
;;     ((564 . 566)  . font-lock-comment-face)
;;     ((567 . 576)  . font-lock-comment-face) ;
;;     ((611 . 614)  . font-lock-string-face)
;;     ((615 . 619)  . font-lock-comment-face)
;;     ((626 . 629)  . font-lock-string-face)
;;     ((630 . 634)  . font-lock-comment-face)
;;     ((641 . 645)  . font-lock-string-face)
;;     ((646 . 650)  . font-lock-comment-face)
;;     ((657 . 661)  . font-lock-string-face)
;;     ((662 . 666)  . font-lock-comment-face)
;;     ((673 . 682)  . font-lock-string-face)
;;     ((683 . 700)  . font-lock-comment-face)))

;;  ;; varargs inside proc
;;  (test-faces-by-range
;;   "should highlight varargs inside proc’s args correctly"
;;   (test-concat-dir "tests/syntax/varargs.nim")
;;   '(((55 . 61) . font-lock-type-face)))


;;  ;; backticks in comment
;;  (test-faces-by-range
;;   "should highlight words inside backticks correctly"
;;   (test-concat-dir "tests/syntax/backtick.nim")
;;   '(((99 . 116) . font-lock-constant-face)
;;     ((122 . 141) . font-lock-constant-face)
;;     ((147 . 164) . font-lock-constant-face)
;;     ((170 . 189) . font-lock-constant-face)
;;     ((195 . 212) . font-lock-constant-face)
;;     ((218 . 237) . font-lock-constant-face)
;;     ((247 . 264) . font-lock-constant-face)
;;     ((270 . 289) . font-lock-constant-face)
;;     ((321 . 338) . font-lock-constant-face)
;;     ((340 . 359) . font-lock-constant-face)))

;;  ;; $# and $[1-9]
;;  (test-faces-by-range
;;   "should highlight $# and $[1-9] inside string correctly"
;;   (test-concat-dir "tests/syntax/format.nim")
;;   '(((84 . 85) . font-lock-preprocessor-face)
;;     ((89 . 90) . font-lock-preprocessor-face)
;;     ((94 . 95) . font-lock-preprocessor-face)))

;;  ;; after ‘is’ operator and ‘distinct’
;;  (test-faces-by-range
;;   "should highlight after ‘is’ operator correctly"
;;   (test-concat-dir "tests/syntax/test_is_and_distinct.nim")
;;   '(((132 . 142) . font-lock-type-face)
;;     ((202 . 212) . font-lock-type-face)
;;     ((282 . 293) . font-lock-type-face)))

;;  (test-faces-by-range
;;   "should highlight single line comment correctly"
;;   ;; Note this test was added because I got wrong highlight
;;   ;; only nimscript file, so don’t change the extension name.
;;   (test-concat-dir "tests/syntax/single_comment.nims")
;;   '(((1 . 70) . font-lock-comment-face)
;;     ((73 . 76) . font-lock-keyword-face)))
 ) ; end of describe function

;; (require 'nimscript-mode)
;; (describe
;;  "Syntax nimscript-mode"
;;  (before-each
;;   (set-buffer (get-buffer-create "*Test*"))
;;   (erase-buffer)
;;   (nimscript-mode))

;;  (after-each
;;   (kill-buffer (get-buffer-create "*Test*")))

;;  (test-faces-by-range
;;   "should highlight NimScript keywords correctly"
;;   (test-concat-dir "tests/syntax/test_nimscript.nims")
;;   '(((38  . 41)  . font-lock-variable-name-face) ; mode
;;     ((45  . 54)  . font-lock-type-face)          ; ScriptMode
;;     ((78  . 81)  . font-lock-keyword-face)       ; task
;;     ((83  . 87)  . font-lock-builtin-face)       ; build
;;     ((145 . 149) . font-lock-builtin-face)       ; tests
;;     ((207 . 211) . font-lock-builtin-face)       ; bench
;;     ((259 . 262) . font-lock-keyword-face)))     ; exec
;;  )

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; test-syntax.el ends here
