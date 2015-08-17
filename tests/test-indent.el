(require 'nim-mode)

(defun file-to-string (filename)
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (buffer-string)))

(describe
 "Indentation"
 (cl-defun indent-and-compare (test-string file-name &key (uncompleted-indent 4))
   (lexical-let ((act (concat file-name "-actual.nim")) (exp (concat file-name "-expected.nim"))
                 (indent uncompleted-indent))
     (it test-string
         (setq nim-uncompleted-condition-indent indent)
         (insert-file-contents-literally act)
         (indent-region (point-min) (point-max))

         (expect (buffer-string)
                 :to-equal
                 (file-to-string exp)))))

 (before-each
  (set-buffer (get-buffer-create "*Test*"))
  (erase-buffer)
  (nim-mode))

 (after-each
  (kill-buffer (get-buffer-create "*Test*")))

 (indent-and-compare
  "should indent #30 correctly"
  "tests/indents/30")

 (indent-and-compare
  "should indent for x, y in foo(): correctly"
  "tests/indents/for-pairs")

 (indent-and-compare
  "should dedent let/comments correctly"
  "tests/indents/line-checking")

 (indent-and-compare
  "should indent import statement's libraries correctly"
  "tests/indents/import-statement")

 (indent-and-compare
  "should indent after ‘object’, ‘enum’, ‘tupel’, and ‘object of’ correctly"
  "tests/indents/line-end-indent")

 (indent-and-compare
  "shouldn't dedent too much after let"
  "tests/indents/after-let")

 (indent-and-compare
  "should indent a nested statement correctly"
  "tests/indents/nested-statement")

 (indent-and-compare
  "should indent case statement correctly"
  "tests/indents/case-stmt")

 (indent-and-compare
  "should indent when statement correctly"
  "tests/indents/when-stmt")

 (indent-and-compare
  "should indent after uncompleted condition correctly by default value"
  "tests/indents/uncompleted-condition"
  :uncompleted-indent 4)

 (indent-and-compare
  "should indent after uncompleted condition correctly by stmt+1 option"
  "tests/indents/uncompleted-condition-stmt+1"
  :uncompleted-indent 'stmt+1)

 (indent-and-compare
  "should indent after colon correctly"
  "tests/indents/indent-after-colon")

 )                                      ; keep this here for less changes

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; test-indent.el ends here
