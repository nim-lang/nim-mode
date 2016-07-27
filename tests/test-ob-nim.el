;;; test-ob-nim.el --- Tests for ob-nim.el           -*- lexical-binding: t; -*-

;; Copyright (C) 2016

;; Author:  <lompik@oriontabArch>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'nim-mode)

(describe
 "ob-nim tests"
 (before-each
  (set-buffer (get-buffer-create "*Test*"))

  (progn
    (setq org-confirm-babel-evaluate nil)
    (org-babel-do-load-languages 'org-babel-load-languages
                                 '((emacs-lisp . t) (nim . t) (org . t))))
  (erase-buffer)

  (org-mode))

 (after-each
  (kill-buffer (get-buffer-create "*Test*")))

 ;; (it "should process `begin_src' headers
;; "
;;      (insert "
;; #+header: :var x = 3
;; #+begin_src nim :import sequtils strutils :define release ssl
;; when defined(release):
;;   when defined(ssl):
;;     var temp = @[1, 2].map (proc(i:int ) : string= $(i*x) )
;;     echo temp.join(\"/\")
;; #+end_src")
;;      (goto-char 0)
;;      (org-babel-next-src-block)
;;      (expect (org-babel-execute-src-block) :to-equal "3/6"))

 (it "should convert table with no colname
"
     (insert "#+name: eg
| col1 | col2 |
|------+------|
| a    | 1    |
| b    | 2.0  |
#+header: :colnames no
#+header: :var x = eg
#+begin_src nim
import strutils
proc print_2d [I1,I2,T2](x:array[I1,array[I2,T2]]): void =
  for i in x:
    echo i.join(\" \")
print_2d(x)
#+end_src")
     (goto-char 0)
     (org-babel-next-src-block)
     (expect (org-babel-execute-src-block) :to-equal '(("col1" "col2") ("a" 1) ("b" 2.0))))

 (it "should convert table with colname
"
     (insert "#+name: eg
| col1 | col2 |
|------+------|
| a    | 1    |
| b    | 2.0  |
#+header: :colnames yes
#+header: :var temp = eg
#+begin_src nim
import strutils
for j in temp[\"col1\"].low..temp[\"col1\"].high:
  var line = \"\"
  for i in temp.keys():
    line &= $temp[i][j] & \" \"
  echo line
#+end_src")
     (goto-char 0)
     (org-babel-next-src-block)
     (expect (org-babel-execute-src-block) :to-equal '(("col1" "col2") hline ("a" 1) ("b" 2.0))))
 )
(provide 'test-ob-nim)
;;; test-ob-nim.el ends here
