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

 (defun test-faces (test-string file-name faces)
   (lexical-let ((file-name file-name) (faces faces))
     (it test-string
         (insert-file-contents-literally file-name)
         (font-lock-default-fontify-buffer)
         (dolist (pos-face faces)
           (expect
            (get-text-property (car pos-face) 'face)
            :to-equal
            (cdr pos-face))))))

 (defun test-faces-by-range (test-string file-name spec &optional not)
   (lexical-let ((file-name file-name) (spec spec) (not not))
     (it test-string
       (insert-file-contents-literally file-name)
       (font-lock-default-fontify-buffer)
       (cl-loop for (place . expected-face) in spec
                for start = (car place)
                for end   = (cdr place)
                do (test-helper-range-expect start end expected-face not)))))

 (defun test-helper-range-expect (start end face &optional not)
   "Expect FACE between START and END positions."
   (cl-loop for pos from start to end
            do (expect
                (get-text-property pos 'face)
                (when not :not)
                :to-equal
                face)))

 (defun test-characters (test-string file-name)
   (lexical-let ((file-name file-name))
     (it test-string
         (insert-file-contents-literally file-name)
         (font-lock-default-fontify-buffer)
         (goto-char (point-min))
         (search-forward "testCharacters: set[char] = {\n    ")
         (let* ((limit (+ 128 22 128)) ; to prevent eternal loop
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
           ))))

 (defun test-double-quote-and-next-line (test-string file-name start-strings)
   (lexical-let ((file-name file-name)
                 (start-strings start-strings)
                 (check-highlight
                  (lambda (string)
                    (goto-char (point-min))
                    (if (search-forward string nil t)
                        (test-helper-range-expect (point) (1- (point-at-eol)) 'font-lock-string-face)
                      (error (format "Failed to find start string: %s" string)))
                    (line-move 1)
                    ;; comment line should not be highlighted by 'font-lock-string-face
                    (test-helper-range-expect (1+ (point-at-bol)) (1- (point-at-eol)) 'font-lock-comment-face))))
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

 (test-double-quote-and-next-line
  "should highlight double quoted string with single quotes"
  (test-concat-dir "tests/syntax/string.nim")
  '("var endOfQuote = "         "var endOfQuote2 = "
    "var beginningOfQuote = "   "var beginningOfQuote2 = "
    "var enclosedQuote = "      "var enclosedQuote2 = "
    "var escapedSingleQuote = " "var escapedSingleQuote2 = "
    "var escapedDoubleQuote = " "var escapedDoubleQuote2 = "))

 ;; Character
 (test-characters
  "should highlight characters correctly"
  (test-concat-dir "tests/syntax/char.nim"))

 ;; Number
 (test-faces-by-range
  "should not highlight numbers by string-face"
  (test-concat-dir "tests/syntax/number.nim")
  '(((1 . 167) . font-lock-string-face))
  t)

 ) ; end of describe function
