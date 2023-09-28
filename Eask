(package "nim-mode"
         "0.4.2"
         "A major mode for the Nim programming language")

(website-url "https://github.com/nim-lang/nim-mode")
(keywords "nim" "languages")

(package-file "nim-mode.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "24.4")
(depends-on "epc" "0.1.1")
(depends-on "let-alist" "1.0.1")
(depends-on "commenter" "0.5.1")
(depends-on "flycheck-nimsuggest" "0.8.1")

(development
 (depends-on "buttercup")
 (depends-on "let-alist")
 (depends-on "cl-lib"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
