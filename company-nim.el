
;;; Commentary

;; please delete following configuration. Those are no longer required.
;;
;; (add-to-list 'company-backends
;;               '(company-nim :with company-nim-builtin))
;;
;; company-mode support is done using completion-at-point, which is
;; Emacs’s default feature. (file is nim-capf.el)
;;
;;; Code:

(define-obsolete-function-alias 'company-nim 'ignore
  "please delete related configuration to ‘company-nim’.
This is no longer required for company-mode.")

(define-obsolete-function-alias 'company-nim-builtin 'ignore
  "please delete related configuration to ‘company-nim-builtin’.
This is no longer required for company-mode.")

(provide 'company-nim)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; company-nim.el ends here
