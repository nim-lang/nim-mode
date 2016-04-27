;;; ob-nim.el --- Babel Functions for C and Similar Languages -*- lexical-binding: t; -*-

;; Author: Lompik
;; Keywords: literate programming, reproducible research

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; Org-Babel support for evaluating nim code (based on ob-C).
;;
;; very limited implementation:
;; - currently only support :results output
;; - not much in the way of error feedback

;;; Code:

(require 'org)
(require 'ob)

(declare-function org-entry-get "org"
		  (pom property &optional inherit literal-nil))
(declare-function org-remove-indentation "org" (code &optional n))

(defcustom org-babel-nim-compiler "nim c"
  "Command used to compile a nim source code file into an executable.
Parameter may be used, like nim cpp"
  :group 'org-babel
  :version "24.3"
  :type 'string)

(defun org-babel-execute:nim (body params)
  "Execute a block of nim code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (org-babel-nim-execute body params))

(defun org-babel-expand-body:nim (body params)
  "Expand a block of nim code with org-babel according to its
header arguments."
  (org-babel-nim-expand-nim body params))

(defun org-babel-nim--sanitize-file-name (file)
  "Remove `-' from file name as invalid for nim compiler"
  (concat
   (file-name-directory file)
   (replace-regexp-in-string "-"
                             "_"
                             (file-name-base
                              file))))

(defun org-babel-nim-execute (body params)
  "This function should only be called by `org-babel-execute:nim'."
  (let* ((tmp-bin-file (org-babel-nim--sanitize-file-name
                        (org-babel-temp-file
                         "nim_src_" )))
	 (tmp-src-file (concat tmp-bin-file
			       ".nim"))
	 (cmdline (cdr (assoc :cmdline params)))
	 (cmdline (if cmdline (concat " " cmdline) ""))
	 (define (org-babel-read
		  (or (cdr (assoc :define params))
		      (org-entry-get nil "define" t))
		  nil))
	 (define (if (stringp define) (split-string define " ") nil))
	 (define (if define
		     (mapconcat
		      (lambda (inc) (format "-d:%s" inc)) define
		      " ")
		   ""))
	 (flags (cdr (assoc :flags params)))
	 (flags (mapconcat 'identity
			   (if (listp flags) flags (list flags)) " "))
	 (libs (org-babel-read
		(or (cdr (assq :libs params))
		    (org-entry-get nil "libs" t))
		nil))
	 (libs (mapconcat #'identity
			  (if (listp libs) libs (list libs))
			  " "))
	 (full-body
	  (org-babel-nim-expand-nim   body params)))
    (with-temp-file tmp-src-file (insert full-body))
    (org-babel-eval
     (format "%s %s %s -o:%s %s %s"
	     org-babel-nim-compiler
	     define
	     flags
	     (org-babel-process-file-name tmp-bin-file)
	     (org-babel-process-file-name tmp-src-file)
	     libs)
     "")
    (let ((results
	   (org-babel-eval
	    (concat tmp-bin-file cmdline)
	    "")))
      (when results
	(setq results (org-babel-trim (org-remove-indentation results)))
	(org-babel-reassemble-table
	 (org-babel-result-cond (cdr (assoc :result-params params))
	   (org-babel-read results t)
	   (let ((tmp-file (org-babel-temp-file "c-")))
	     (with-temp-file tmp-file (insert results))
	     (org-babel-import-elisp-from-file tmp-file)))
	 (org-babel-pick-name
	  (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
	 (org-babel-pick-name
	  (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params)))))
      )))

(defun org-babel-nim-expand-nim (body params)
  "Expand a block of nim code with org-babel according to
its header arguments."
  (let* ((vars (if (boundp 'org-babel--get-vars)
                   (org-babel--get-vars params)
                 (mapcar #'cdr (org-babel-get-header params :var))))
         (colnames (if (boundp 'org-babel--get-vars)
                       (cdr (assq :colname-names params))
                     (cdar (org-babel-get-header params :colname-names))))
	 (colnames (cdr (assq :colname-names params)))
         (imports (org-babel-read
                   (or (cdr (assoc :import params))
                       (org-entry-get nil "import" t))
                   nil))
         (imports (if (stringp imports) (split-string imports " ") nil))
         (imports (if imports
                      (if (listp imports) imports (list imports))
                    nil)))
    (if colnames (add-to-list 'imports 'tables))
    (if colnames (add-to-list 'imports 'strutils))
    (mapconcat 'identity
	       (list
		;; imports
		(if imports
		    (mapconcat
		     (lambda (inc) (format "import %s" inc))
		     imports "\n"))
		;; variables
		(if colnames
		    ;;with colnames .. as tables
		    (let ((vc (mapcar* (lambda (el)
					 (cons el (assoc (car el) colnames )))
				       vars)))
		      (mapconcat 'org-babel-nim-var-to-nim-cn vc  "\n"))
		  ;; else as arrays
		  (mapconcat 'org-babel-nim-var-to-nim vars "\n"))
		;; table sizes
		(mapconcat 'org-babel-nim-table-sizes-to-nim vars "\n")
		;; tables headers utility
		;; body
		body "\n") "\n")))


(defun org-babel-prep-session:nim (_session _params)
  "This function does nothing as a good nim repl is required"
  (error "No support for sessions yet"))

(defun org-babel-load-session:nim (_session _body _params)
  "This function does nothing as a good nim repl is required"
  (error "No support for sessions yet"))

;; helper functions

(defun org-babel-nim-format-val (type val)
  "Handle the FORMAT part of TYPE with the data from VAL."
  (let ((format-data (cadr type)))
    (if (stringp format-data)
	(cons "" (format format-data val))
      (funcall format-data val))))

(defun org-babel-nim-val-to-nim-type (val)
  "Determine the type of VAL.
Return a list (TYPE-NAME FORMAT).  TYPE-NAME should be the name of the type.
FORMAT can be either a format string or a function which is called with VAL.
If VAL is a table it is exported as a `array' of `array' in nim."
  (let* ((basetype (org-babel-nim-val-to-base-type val))
	 (type
	  (case basetype
	    (integerp '("int" "%d"))
	    (floatp '("float" "%f"))
	    (stringp '("string" "\"%s\""))
	    (t (error "Unknown type %S" basetype))))
	 (nim_nil
	  (case basetype
	    (integerp 0)
	    (floatp 0.0)
	    (stringp "")
	    (t (error "Unknown type %S" basetype)))))
    (cond
     ((integerp val) type) ;; an integer declared in the #+begin_src line
     ((floatp val) type) ;; a numeric declared in the #+begin_src line
     ((and (listp val) (listp (car val))) ;; a table
      `(,(car type)
	(lambda (val)
	  (cons
	   (format "array[0..%d, array[0..%d, %s]]" (1- (length val)) (1- (length (car val))) ,(car type))
	   (concat
	    "[\n"
	    (mapconcat
	     (lambda (v)
	       (concat
		" ["
		(mapconcat (lambda (w) (format ,(cadr type) (if w w nim_nil))) v ",")
		"]" ))
	     val
	     ",\n")
	    "\n]" )))))
     ((or (listp val) (vectorp val)) ;; a list declared in the #+begin_src line
      `(,(car type)
	(lambda (val)
	  (cons
	   (format "[%d]" (length val))
	   (concat
	    "["
	    (mapconcat (lambda (v) (format ,(cadr type) v)) val ",")
	    "]")))))
     (t ;; treat unknown types as string
      type))))

(defun org-babel-nim-val-to-base-type (val)
  "Determine the base type of VAL which may be
`integerp' if all base values are integers
`floatp' if all base values are either floating points or integers
`stringp' otherwise."
  (cond
   ((integerp val) 'integerp)
   ((floatp val) 'floatp)
   ((or (listp val) (vectorp val))
    (let ((type nil))
      (mapc (lambda (v)
	      (case (org-babel-nim-val-to-base-type v)
		(stringp (setq type 'stringp))
		(floatp
		 (if (or (not type) (eq type 'integerp))
		     (setq type 'floatp)))
		(integerp
		 (unless type (setq type 'integerp)))))
	    val)
      type))
   (t 'stringp)))

(defun org-babel-nim-var-to-nim (pair)
  "Convert an elisp val into a string of nim code specifying a var
of the same value."
  ;; TODO list support
  (let ((var (car pair))
	(val (cdr pair)))
    (when (symbolp val)
      (setq val (symbol-name val))
      (when (= (length val) 1)
	(setq val (string-to-char val))))
    (let* ((type-data (org-babel-nim-val-to-nim-type val))
	   (type (car type-data))
	   (formated (org-babel-nim-format-val type-data val))
	   (suffix (car formated))
	   (data (cdr formated)))
      (format "var %s: %s = %s"
	      var
	      (if (string= suffix "") type suffix)
	      data))))

(defun org-babel-nim-format-val-cn (type val coln)
  "Handle the FORMAT part of TYPE with the data from VAL and COLN."
  (let ((format-data (cadr type)))
    (if (stringp format-data)
	(cons "" (format format-data val))
      (funcall format-data (apply #'mapcar* #'list val)	       coln))))

(defun org-babel-nim-val-to-nim-type-cn (val colnames)
  "Determine the type of VAL.
Return a list (TYPE-NAME FORMAT).  TYPE-NAME should be the name of the type.
FORMAT can be either a format string or a function which is called with VAL.
If VAL is a table it is exported a `Table' of `array' with COLNAMES as keys."
  (let* ((basetype (org-babel-nim-val-to-base-type val))
	 (type
	  (case basetype
	    (integerp '("int" "%d"))
	    (floatp '("float" "%f"))
	    (stringp '("string" "\"%s\""))
	    (t (error "Unknown type %S" basetype))))
	 (nim_nil
	  (case basetype
	    (integerp 0)
	    (floatp 0.0)
	    (stringp "")
	    (t (error "Unknown type %S" basetype)))))
    (cond
     ((integerp val) type) ;; an integer declared in the #+begin_src line
     ((floatp val) type) ;; a numeric declared in the #+begin_src line
     ((and (listp val) (listp (car val))) ;; a table
      `(,(car type)
	(lambda (val coln)
	  (cons
	   ;;(format "Table[string,seq[%s]]"  ,(car type))

	   (format "Table[string,array[0..%d,%s]]" (- (length (car val)) 1) ,(car type))
	   (concat
	    "{\n"
	    (mapconcat
	     (lambda (v)
	       (concat
		(format "  \"%s\"" (pop coln))
		": ["
		(mapconcat (lambda (w) (format ,(cadr type) (if w w nim_nil))) v ",")
		"]" ))
	     val
	     ",\n")
	    "  }.toTable" )))))
     ((or (listp val) (vectorp val)) ;; a list declared in the #+begin_src line
      `(,(car type)
	(lambda (val)
	  (cons
	   (format "[%d]" (length val))
	   (concat
	    "["
	    (mapconcat (lambda (v) (format ,(cadr type) v)) val ",")
	    "]")))))
     (t ;; treat unknown types as string
      type))))

(defun org-babel-nim-var-to-nim-cn (valscn)
  "Convert an elisp val into a string (with colnames) of nim code specifying a var
of the same value."
  ;; TODO list support
  (let* ((colnames (cddr valscn))
	 (pair (car valscn))
	 (var (car pair))
	 (val (cdr pair)))
    (when (symbolp val)
      (setq val (symbol-name val))
      (when (= (length val) 1)
	(setq val (string-to-char val))))
    (let* ((type-data (org-babel-nim-val-to-nim-type-cn val colnames))
	   (type (car type-data))
	   (formated (org-babel-nim-format-val-cn type-data val colnames))
	   (suffix (car formated))
	   (data (cdr formated)))
      (format "var %s: %s = %s"
	      var
	      (if (string= suffix "") type suffix)
	      data))))

(defun org-babel-nim-table-sizes-to-nim (pair)
  "Create constants of table dimensions, if PAIR is a table."
  (when (listp (cdr pair))
    (cond
     ((listp (cadr pair)) ;; a table
      (concat
       (format "const %s_rows = %d" (car pair) (length (cdr pair)))
       "\n"
       (format "const %s_cols = %d" (car pair) (length (cadr pair)))))
     (t ;; a list declared in the #+begin_src line
      (format "const %s_cols = %d" (car pair) (length (cdr pair)))))))

(provide 'ob-nim)

;;; ob-nim.el ends here
