(require 'nim-mode)

(describe
 "Syntax"
 (before-all
   (setq test-syntax-dir (concat (locate-dominating-file buffer-file-name ".git"))))

 (before-each
  (set-buffer (get-buffer-create "*Test*"))
  (erase-buffer)
  (nim-mode))

 (after-each
  (kill-buffer (get-buffer-create "*Test*")))

 (defun test-concat-dir (filepath)
   (if noninteractive
       filepath
     (concat test-syntax-dir filepath)))

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

 ;; You can check which faces are at a position with
 ;; (text-properties-at pos (get-buffer "file.nim"))
 (test-faces
  "should highlight strings"
  (test-concat-dir "tests/syntax/string.nim")
  '((10 . font-lock-string-face)
    (5 . font-lock-variable-name-face))))
