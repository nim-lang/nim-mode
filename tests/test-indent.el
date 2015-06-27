(require 'nim-mode)

(defun file-to-string (filename)
  (with-temp-buffer
    (insert-file-literally filename)
    (buffer-string)))

(describe
 "Indentation"
 (defun indent-and-compare (test-string actual expected)
   (lexical-let ((act actual) (exp expected))
     (it test-string
         (insert-file-literally act)
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
  "tests/samples/30-actual.nim"
  "tests/samples/30-expected.nim")

 (indent-and-compare
  "should indent for x, y in foo(): correctly"
  "tests/samples/for-pairs-actual.nim"
  "tests/samples/for-pairs-expected.nim"))
