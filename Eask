;; -*- mode: eask; lexical-binding: t -*-

(package "nim-mode"
         "0.4.2"
         "A major mode for the Nim programming language")

(website-url "https://github.com/nim-lang/nim-mode")
(keywords "nim" "languages")

(package-file "nim-mode.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "27.1")
(depends-on "epc" "0.1.1")
(depends-on "let-alist" "1.0.1")
(depends-on "commenter" "0.5.1")

(development
 (depends-on "buttercup"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
