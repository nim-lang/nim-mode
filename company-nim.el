
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

(make-obsolete-variable
 'company-nim nil
 "please check README page, but no harm for deleting this variable")

(make-obsolete-variable
 'company-nim-builtin nil
 "please check README page, but no harm for deleting this variable")

(define-obsolete-function-alias 'company-nim nil ""
  "please delete related configuration to ‘company-nim’.
This is no longer required for company-mode.")

(define-obsolete-function-alias 'company-nim-builtin nil ""
  "please delete related configuration to ‘company-nim-builtin’.
This is no longer required for company-mode.")

(provide 'company-nim)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; company-nim.el ends here
