;;; test-helper.el --- -*- lexical-binding: t; -*-

(require 'nim-mode)
(require 'cl-lib)

;; Test functions
(defun test-concat-dir (filepath)
   (if noninteractive
       filepath
     (defvar-local test-syntax-dir (concat (locate-dominating-file buffer-file-name ".git")))
     (concat test-syntax-dir filepath)))

(defun prepare-file (file)
  (when file
    (insert-file-contents-literally (test-concat-dir (concat "tests/syntax/" file)))
    (font-lock-default-fontify-buffer)))

(cl-defun face-at-point-of (pos &key on)
  "Return point of the face."
  (prepare-file on)
  (get-text-property pos 'face))

(cl-defun range-of-faces-between (pos1 pos2 &key on)
  "Return face between POS1 and POS2."
  (prepare-file on)
  (cl-loop for i from pos1 to pos2
           collect (get-text-property i 'face)))

(defun the-range-of (faces)
  (1- (length faces)))

(defun diff-of (pos1 pos2)
  (- (max pos1 pos2) (min pos1 pos2)))

(cl-defun assert-highlight-between (pos1 pos2 &key on match)
  (let ((faces
         (cl-loop with fs = (range-of-faces-between pos1 pos2 :on on)
                  for face in (delq nil fs)
                  if (eq match face)
                  collect face
                  else collect 'unexpected-face)))
    (expect faces :to-contain match)
    (expect faces :not :to-contain 'unexpected-face)
    (expect (the-range-of faces) :to-be (diff-of pos1 pos2))))

(cl-defun assert-highlights-between (ranges &key on match)
  (cl-loop for (pos1 . pos2) in ranges
           do (assert-highlight-between pos1 pos2 :on on :match match)))

(defun check-highlight (string)
  (goto-char (point-min))
  (if (search-forward string nil t)
      (assert-highlight-between
       (point) (1- (point-at-eol)) :on nil :match 'font-lock-string-face)
    (error (format "Failed to find start string: %s" string)))
  (when (line-move 1 t)
    ;; comment line should not be highlighted by 'font-lock-string-face
    (assert-highlight-between
     (1+ (point-at-bol)) (1- (point-at-eol)) :on nil :match 'font-lock-comment-face)))

(cl-defun expect-string (&key on search-strings)
  "WIP"
  (prepare-file on)
  (cl-loop for s in search-strings do (check-highlight s)))


;; (defun test-faces (test-string file-name faces)
;;   (it test-string
;;       (insert-file-contents-literally file-name)
;;       (font-lock-default-fontify-buffer)
;;       (dolist (pos-face faces)
;;         (expect
;;          (get-text-property (car pos-face) 'face)
;;          :to-equal
;;          (cdr pos-face)))))

;; (defun test-faces-by-range (test-string file-name spec &optional not)
;;   (it test-string
;;       (insert-file-contents-literally file-name)
;;       (font-lock-default-fontify-buffer)
;;       (cl-loop for (place . expected-face) in spec
;;                for start = (car place)
;;                for end   = (cdr place)
;;                do (test-helper-range-expect start end expected-face not))))

;; (defun test-helper-range-expect (start end face &optional not)
;;   "Expect FACE between START and END positions."
;;   (cl-loop for pos from start to end
;;            do (expect
;;                (get-text-property pos 'face)
;;                (when not :not)
;;                :to-equal
;;                face)))

;; FIXME: still broken
(defun test-characters (test-string file-name)
  (it test-string
      (prepare-file file-name)
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
