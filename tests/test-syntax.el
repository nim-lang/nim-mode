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
        "var rawString = r" "var rawStringIssue210 = r"
        ;; #212
        "var rawStringWithBackslash = r"
        "var rawStringWithBackslash2 = R2D2"
        "var rawStringWithBackslash3 = sql"
        "var rawStringWithBackslash4 = "
        "var rawStringWithBackslash5 = r"
        "var rawStringWithBackslash6 = sql")))

 (it "should highlight numbers"
     (assert-highlights-between
      '(;; number part
        (21   . 23) (43   . 43) (64   . 65)
        (87   . 88) (110  . 111) (133  . 133) (158  . 158)
        ;; type part
        (24 . 25) (44 . 46) (66 . 69) (89 . 92)
        (112 . 115) (137 . 140) (162 . 165))
      :on "number.nim" :match 'nim-font-lock-number-face))

 (it "should highlight pragmas"
     (assert-highlights-between
      '((31 . 40) (79 . 85))
      :on "pragma.nim" :match 'nim-font-lock-pragma-face))

 (it "should highlight docstring"
     (assert-highlights-between
      '((56 . 99) (128 . 175))
      :on "docstring.nim" :match font-lock-doc-face))

 (it "should highlight multi line comment and doc comment"
     (prepare-file "multiline_comment.nim")
     (assert-highlights-between
      '((1 . 5) (77 . 78) (80 . 185) (564 . 566) (567 . 576) (615 . 619)
        (630 . 634) (646 . 650) (662 . 666) (683 . 700))
      :match font-lock-comment-face)
     (assert-highlights-between
      '((48 . 75) (192 . 235) (545 . 553) (611 . 614) (626 . 629)
        (641 . 645) (657 . 661) (673 . 682))
      :match font-lock-string-face)
     (assert-highlights-between
      '((237 . 453) (482 . 536))
      :match font-lock-doc-face)
     ;; might be added eventually
     ;; (assert-highlights-between
     ;;  '()
     ;;  :match font-lock-comment-delimiter-face)
     )

 (it "should highlight varargs inside proc’s args correctly"
     (assert-highlight-between 55 61 :on "varargs.nim"
                               :match font-lock-type-face))

 (it "should highlight words inside backticks correctly"
     (assert-highlights-between
      '((100 . 115) (124 . 139) (148 . 163) (172 . 187)
        (196 . 211) (220 . 235) (248 . 263) (272 . 287)
        (322 . 337) (342 . 357))
      :on "backtick.nim"
      :match font-lock-constant-face))

 ;; $# and $[1-9]
 (it "should highlight $# and $[1-9] inside string correctly"
  (assert-highlights-between
   '((84 . 85) (89 . 90) (94 . 95))
   :on "format.nim" :match font-lock-preprocessor-face))

 ;; after ‘is’ operator and ‘distinct’
 (it "should highlight after ‘is’ operator correctly"
  (assert-highlights-between
   '((132 . 142) (202 . 212) (282 . 293))
   :on "test_is_and_distinct.nim" :match font-lock-type-face))

 (it "should highlight single line comment correctly"
  ;; Note this test was added because I got wrong highlight
  ;; only nimscript file, so don’t change the extension name.
  (prepare-file "single_comment.nims")
  (assert-highlight-between 1 70  :match font-lock-comment-face)
  (assert-highlight-between 73 76 :match font-lock-keyword-face))

 ;; Character
 (it "should highlight characters correctly"
     (let* ((chars (collect-char-points :on "char.nim"))
            (char-pos   (car chars))
            (after-char (cdr chars)))
       (assert-highlights-between char-pos :match font-lock-string-face)
       (assert-highlights-between after-char :notmatch font-lock-string-face)))

 ) ; end of describe function

(describe "Syntax nimscript-mode"
 (before-each
  (set-buffer (get-buffer-create "*Test*"))
  (erase-buffer)
  ;; may need to turn off this to adapt triple double quotes.
  (prettify-symbols-mode 0)
  (nimscript-mode))

 (after-each
  (kill-buffer (get-buffer-create "*Test*")))

 (it "should highlight NimScript keywords correctly"
     (prepare-file "test_nimscript.nims")
     (assert-highlights-between
      '((78 . 81) (83  . 87) (145 . 149) (207 . 211) (259 . 262))
      :match font-lock-builtin-face)
     (assert-highlight-between 38 41 :match font-lock-variable-name-face)
     (assert-highlight-between 45 54 :match font-lock-type-face))

 ) ; end of describe for nimscript

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; test-syntax.el ends here
