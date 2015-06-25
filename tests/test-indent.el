(defun file-to-string (filename)
  (with-temp-buffer
    (insert-file-literally filename)
    (buffer-string)))

(describe "Indentation"
  (before-each
    (set-buffer (get-buffer-create "*Test*"))
    (erase-buffer)
    (nim-mode))

  (after-each
    (kill-buffer (get-buffer-create "*Test*")))

  (it "should indent #30 correctly"
    (insert-file-literally "tests/samples/actual-30.nim")
    (indent-region (point-min) (point-max))

    (expect (buffer-string)
            :to-equal
            (file-to-string "tests/samples/expected-30.nim"))))
