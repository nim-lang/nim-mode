;;; nim-rx.el ---

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

(require 'rx)
(require 'cl-lib)
(require 'nim-vars)

(eval-and-compile
  (defvar nim-rx-constituents
    (let* ((constituents1
            (cl-loop for (sym . kwd) in `((keyword           . ,nim-keywords)
                                          (dedenter          . ("elif" "else" "of" "except" "finally"))
                                          (type              . ,nim-types)
                                          (exception         . ,nim-exceptions)
                                          (constant          . ,nim-constants)
                                          (builtin           . ,nim-builtins)
                                          (defun             . ("proc" "method" "converter" "iterator" "template" "macro"))
                                          (block-ender       . ("break" "continue" "raise" "return"))
                                          (block-start-defun . ("proc" "method" "converter" "iterator"
                                                                "template" "macro"
                                                                "if" "elif" "else" "when" "while" "for" "case" "of"
                                                                "try" "except" "finally"
                                                                "with" "block"
                                                                "enum" "tuple" "object")))
                     collect (cons sym (apply `((lambda () (rx symbol-start (or ,@kwd) symbol-end)))))))
           (constituents2 `((decl-block . ,(rx symbol-start
                                               (or "type" "const" "var" "let" "import")
                                               symbol-end
                                               (* space)
                                               (or "#" eol)))

                            (symbol-name          . ,(rx (any letter ?_ ?–) (* (any word ?_ ?–))))
                            (do-declaration . ,(rx (minimal-match (* anything)) "do" (minimal-match (* anything)) ":" eol))
                            (dec-number . ,(rx symbol-start
                                               (1+ (in digit "_"))
                                               (opt "." (in digit "_"))
                                               (opt (any "eE") (1+ digit))
                                               (opt "'" (or "i8" "i16" "i32" "i64" "f32" "f64"))
                                               symbol-end))
                            (hex-number . ,(rx symbol-start
                                               (1+ (in xdigit "_"))
                                               (opt "." (in xdigit "_"))
                                               (opt (any "eE") (1+ xdigit))
                                               (opt "'" (or "i8" "i16" "i32" "i64" "f32" "f64"))
                                               symbol-end))
                            (oct-number . ,(rx symbol-start
                                               (1+ (in "0-7_"))
                                               (opt "." (in "0-7_"))
                                               (opt (any "eE") (1+ "0-7_"))
                                               (opt "'" (or "i8" "i16" "i32" "i64" "f32" "f64"))
                                               symbol-end))
                            (bin-number . ,(rx symbol-start
                                               (1+ (in "0-1_"))
                                               (opt "." (in "0-1_"))
                                               (opt (any "eE") (1+ "0-1_"))
                                               (opt "'" (or "i8" "i16" "i32" "i64" "f32" "f64"))
                                               symbol-end))
                            (open-paren           . ,(rx (or "{" "[" "(")))
                            (close-paren          . ,(rx (or "}" "]" ")")))
                            (simple-operator      . ,(rx (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%)))
                            ;; FIXME: rx should support (not simple-operator).
                            (not-simple-operator  . ,(rx
                                                      (not
                                                       (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%))))
                            ;; FIXME: Use regexp-opt.
                            (operator             . ,(rx (or (1+ (in "-=+*/<>@$~&%|!?^.:\\"))
                                                             (and
                                                              symbol-start
                                                              (or
                                                               "and" "or" "not" "xor" "shl"
                                                               "shr" "div" "mod" "in" "notin" "is"
                                                               "isnot")
                                                              symbol-end))))
                            ;; FIXME: Use regexp-opt.
                            (assignment-operator  . ,(rx (* (in "-=+*/<>@$~&%|!?^.:\\")) "="))
                            (string-delimiter . ,(rx (and
                                                      ;; Match even number of backslashes.
                                                      (or (not (any ?\\ ?\")) point
                                                          ;; Quotes might be preceded by a escaped quote.
                                                          (and (or (not (any ?\\)) point) ?\\
                                                               (* ?\\ ?\\) (any ?\")))
                                                      (* ?\\ ?\\)
                                                      ;; Match single or triple quotes of any kind.
                                                      (group (or  "\"" "\"\"\"")))))
                            (character-delimiter
                             ;; Implemented with
                             ;; http://nim-lang.org/docs/manual.html#lexical-analysis-character-literals
                             . ,(rx
                                 (group "'")
                                 (or
                                  ;; escaped characters
                                  (and ?\\ (or (in "a-c" "e" "f" "l" "r" "t" "v"
                                                   "\\" "\"" "'" "0-9")
                                               (and "x" (regex "[a-fA-F0-9]\\{2,2\\}"))))
                                  ;; One byte characters(except single quote and control characters)
                                  (eval (cons 'in (list (concat (char-to-string 32) "-" (char-to-string (1- ?\')))
                                                        (concat (char-to-string (1+ ?\')) "-" (char-to-string 126))))))
                                 (group "'")))
                            (coding-cookie . ,(rx line-start ?# (* space)
                                                  (or
                                                   ;; # coding=<encoding name>
                                                   (: "coding" (or ?: ?=) (* space) (group-n 1 (+ (or word ?-))))
                                                   ;; # -*- coding: <encoding name> -*-
                                                   (: "-*-" (* space) "coding:" (* space)
                                                      (group-n 1 (+ (or word ?-))) (* space) "-*-")
                                                   ;; # vim: set fileencoding=<encoding name> :
                                                   (: "vim:" (* space) "set" (+ space)
                                                      "fileencoding" (* space) ?= (* space)
                                                      (group-n 1 (+ (or word ?-))) (* space) ":")))))))
      (append constituents1 constituents2))
    "Additional Nim specific sexps for `nim-rx'.")

  (defmacro nim-rx (&rest regexps)
    "Nim mode specialized rx macro.
This variant of `rx' supports common nim named REGEXPS."
    (let ((rx-constituents (append nim-rx-constituents rx-constituents)))
      (cond ((null regexps)
             (error "No regexp"))
            ((cdr regexps)
             (rx-to-string `(and ,@regexps) t))
            (t
             (rx-to-string (car regexps) t)))))

  (add-to-list 'nim-rx-constituents
               (cons 'block-start (nim-rx (or decl-block block-start-defun do-declaration))))

  ;; Regular expression matching the end of line after with a block starts.
  ;; If the end of a line matches this regular expression, the next
  ;; line is considered an indented block.  Whitespaces at the end of a
  ;; line are ignored.
  (add-to-list 'nim-rx-constituents
               (cons 'line-end-indenters
                     (nim-rx (or "type" "const" "var" "let" "tuple" "object" "enum" ":"
                                 (and defun (* (not (any ?=))) "=")
                                 (and "object" (+ whitespace) "of" (+ whitespace) symbol-name)))))

  ) ; end of eval-and-compile

(provide 'nim-rx)
;;; nim-rx.el ends here
