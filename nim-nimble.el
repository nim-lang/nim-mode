;;; nim-nimble.el --- Providing nimble commands -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'nim-vars)
(require 'nim-compile)
(require 'ansi-color)


;;;###autoload
(defun nimble-run ()
  "Run the current nimble project."
  (interactive)
  (setq default-directory (nim-get-project-root))
  (let ((process
         (start-process "nimble" "*nimble-output*" "nimble" "run")))
    (with-current-buffer (process-buffer process)
      (display-buffer (current-buffer))
      (require 'shell)
      (shell-mode)
      (read-only-mode)
      (set-process-filter process 'comint-output-filter))))



(provide 'nim-nimble)
;;; nim-nimble.el ends here
