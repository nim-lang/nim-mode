(require 'nim-mode)

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

 (defun test-faces (test-string file-name faces &optional not-equal)
   (lexical-let ((file-name file-name) (faces faces))
     (it test-string
         (insert-file-literally file-name)
         (font-lock-default-fontify-buffer)
         (dolist (pos-face faces)
           (expect
            (get-text-property (car pos-face) 'face)
            (when not-equal :not)
            :to-equal
            (cdr pos-face))))))

 (defun test-faces-by-range (test-string file-name spec)
   (lexical-let ((file-name file-name) (spec spec))
     (it test-string
       (insert-file-literally file-name)
       (font-lock-default-fontify-buffer)
       (cl-loop for (place . expected-face) in spec
                for start = (car place)
                for end   = (cdr place)
                do (cl-loop for pos from start to end
                            do (expect
                                (get-text-property pos 'face)
                                :to-equal
                                expected-face))))))

 ;; You can check which faces are at a position with
 ;; (text-properties-at pos (get-buffer "file.nim"))
 (test-faces
  "should highlight strings"
  (test-concat-dir "tests/syntax/string.nim")
  '((10 . font-lock-string-face)
    (5 . font-lock-variable-name-face)))

 (test-faces-by-range
  "should highlight inside here document"
  (test-concat-dir "tests/syntax/string.nim")
  '(((33 . 86) . font-lock-string-face)))

 (test-faces
  "should not highlight number type literal by string face"
  (test-concat-dir "tests/syntax/string.nim")
  '((112 . font-lock-string-face)
    (125 . font-lock-string-face)
    (140 . font-lock-string-face)
    (157 . font-lock-string-face)
    (175 . font-lock-string-face)
    (209 . font-lock-string-face))
  t)

 (test-faces-by-range
  "should highlight char of double quote"
  (test-concat-dir "tests/syntax/string.nim")
  '(((239 . 241) . font-lock-string-face)))

 (test-faces-by-range
  "should highlight comment correctly after char of double quote"
  (test-concat-dir "tests/syntax/string.nim")
  '(((245 . 291) . font-lock-comment-face)))

 ) ; end of describe
