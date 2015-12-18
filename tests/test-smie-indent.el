;;; test-smie-indent.el --- Tests indentation using SMIE -*-lexical-binding:t-*-

;;; Code:

(require 'nim-mode)
(require 'cl-lib)

;; SMIE indentation supports from Emacs 24.4
(when (version<= "24.4" emacs-version)
  (defvar test-dir "tests/indents/SMIE/")
  (defvar temp-file "/tmp/nim-test.nim")
  (describe
   "Indentation (SMIE)"

   (defun SMIE-indent-and-compare (file)
     (if (not (eq (point-min) (point-max)))
         (error "Buffer isn't erased")
       (it (format "should indent %s correctly" (file-name-base file))
           (with-temp-file temp-file
             (erase-buffer)
             (nim-mode)
             (insert-file-contents-literally file)
             (nim-unformat-buffer)
             (set-mark (point-min))
             (goto-char (point-max))
             (call-interactively 'indent-region))
           (expect
            (shell-command-to-string
             (format "diff %s %s" temp-file (shell-quote-argument file)))
            :to-equal ""))))

   (defun nim-unformat-buffer ()
     "Convenience function to make a no indented source file."
     (interactive)
     ;; implemented for non-interactive use, but for debug purpose,
     ;; I added that interactive.
     (let ((message-log-max nil))
       (while (re-search-forward "^ +" nil t)
         (replace-match "" nil nil))))

   (defadvice make-progress-reporter (around prevent-echoing activate)
     "only replace indent region’s echo message"
     (if (equal (ad-get-arg 0)  "Indenting region...")
         (ad-set-arg 0 "..."))
     ad-do-it)

   (defadvice progress-reporter-done (around prevent-echoing activate)
     "Prevent done message during test."
     "")

   (before-each
    (setq nim-use-smie-indent t))

   (after-each
    (kill-buffer (get-buffer-create "*Test*")))

   (cl-loop for file in (directory-files test-dir)
            for f = (format "%s%s" test-dir file)
            if (and (not (member file '("." "..")))
                    (equal "nim" (file-name-extension file)))
            do (SMIE-indent-and-compare f))

   )) ; keep this here for less changes

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; test-smie-indent.el ends here
