(require 'nim-mode)

(describe
 "Syntax"
 (before-each
  (set-buffer (get-buffer-create "*Test*"))
  (erase-buffer)
  (nim-mode))

 (after-each
  (kill-buffer (get-buffer-create "*Test*")))

 (defun test-faces (test-string file-name faces)
   (lexical-let ((file-name file-name) (faces faces))
     (it test-string
         (insert-file-literally file-name)
         (font-lock-default-fontify-buffer)
         (dolist (pos-face faces)
           (expect
            (get-text-property (car pos-face) 'face)
            :to-equal
            (cdr pos-face))))))

 (test-faces
  "should highlight strings"
  "tests/syntax/string.nim"
  '((10 . font-lock-string-face)
    (5 . font-lock-variable-name-face))))
