;;; test-smie-indent.el --- Tests indentation using SMIE -*-lexical-binding:t-*-

;;; Code:

(require 'nim-mode)
(require 'cl-lib)

;; SMIE indentation supports from Emacs 24.4
(when (version<= "24.4" emacs-version)
  (defvar test-dir "tests/indents/SMIE/")
  (defvar temp-file "/tmp/nim-test.nim")

  (defun SMIE-indent-and-compare (file &optional raw)
    (if (not (eq (point-min) (point-max)))
        (error "Buffer isn't erased")
      (it (format "should indent %s correctly" (file-name-base file))
          (with-temp-file temp-file
            (nim-smie-test-insert-and-indent file raw))
          (expect
           (shell-command-to-string
            (format "diff %s %s" temp-file (shell-quote-argument file)))
           :to-equal ""))))

  (defun nim-smie-test-insert-and-indent (&optional file raw)
    (interactive)
    (erase-buffer)
    (nim-mode)
    (insert-file-contents-literally
     (or file (read-from-minibuffer "file :" (buffer-file-name))))
    (unless raw (nim-unformat-buffer))
    (set-mark (point-min))
    (goto-char (point-max))
    (call-interactively 'indent-region))

  ;; just for manually check
  ;; (global-set-key (kbd "C-0") 'nim-smie-test-insert-and-indent)

  (defun nim-unformat-buffer ()
    "Convenience function to make a no indented source file."
    (interactive)
    ;; implemented for non-interactive use, but for debug purpose,
    ;; I added that interactive.
    (let ((message-log-max nil))
      (while (re-search-forward "^ +" nil t)
        (replace-match "" nil nil))))

  (defun nim-indent-test (dir &optional raw)
    (cl-loop for file in (directory-files dir)
             for f = (format "%s%s" dir file)
             if (and (not (member file '("." "..")))
                     (equal "nim" (file-name-extension file)))
             do (SMIE-indent-and-compare f raw)))

  (defadvice make-progress-reporter (around prevent-echoing activate)
    "only replace indent region’s echo message"
    (if (equal (ad-get-arg 0)  "Indenting region...")
        (ad-set-arg 0 "..."))
    ad-do-it)

  (defadvice progress-reporter-done (around prevent-echoing activate)
    "Prevent done message during test."
    "")

  (describe
   "Indentation"
   (after-each (kill-this-buffer))
   (nim-indent-test test-dir))

  ;; This test don’t unformat.
  ;; I intended this to test indentation inside string
  (describe
   "Indentation (raw)"
   (after-each (kill-this-buffer))
   (nim-indent-test (format "%s/raw/" test-dir) t))

  (describe
   "indentation (after break)"
   (before-each
    (setq nim-smie-dedent-after-break '("if" "when" "elif" "else" "finally")))
   (after-each
    (kill-this-buffer)
    (setq nim-smie-dedent-after-break nil))
   (nim-indent-test (format "%s/with-variable/" test-dir) t))
  ) ; keep this here for less changes

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; test-smie-indent.el ends here
