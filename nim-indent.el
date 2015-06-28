;;; nim-indent.el ---

;;; Commentary:

;;

;;; Code:
(require 'nim-helper)

(defcustom nim-indent-trigger-commands
  '(indent-for-tab-command yas-expand yas/expand)
  "Commands that might trigger a `nim-indent-line' call."
  :type '(repeat symbol)
  :group 'nim)

(defvar nim-indent-current-level 0
  "Current indentation level `nim-indent-line-function' is using.")

(defvar nim-indent-levels '(0)
  "Levels of indentation available for `nim-indent-line-function'.")

(defun nim-indent-context ()
  "Get information about the current indentation context.
Context is returned in a cons with the form (STATUS . START).

STATUS can be one of the following:

keyword
-------

:after-comment
 - Point is after a comment line.
 - START is the position of the \"#\" character.
:inside-string
 - Point is inside string.
 - START is the position of the first quote that starts it.
:no-indent
 - No possible indentation case matches.
 - START is always zero.

:inside-paren
 - Fallback case when point is inside paren.
 - START is the first non space char position *after* the open paren.
:inside-paren-at-closing-nested-paren
 - Point is on a line that contains a nested paren closer.
 - START is the position of the open paren it closes.
:inside-paren-at-closing-paren
 - Point is on a line that contains a paren closer.
 - START is the position of the open paren.
:inside-paren-newline-start
 - Point is inside a paren with items starting in their own line.
 - START is the position of the open paren.
:inside-paren-newline-start-from-block
 - Point is inside a paren with items starting in their own line
   from a block start.
 - START is the position of the open paren.

:after-backslash
 - Fallback case when point is after backslash.
 - START is the char after the position of the backslash.
:after-backslash-assignment-continuation
 - Point is after a backslashed assignment.
 - START is the char after the position of the backslash.
:after-backslash-block-continuation
 - Point is after a backslashed block continuation.
 - START is the char after the position of the backslash.
:after-backslash-dotted-continuation
 - Point is after a backslashed dotted continuation.  Previous
   line must contain a dot to align with.
 - START is the char after the position of the backslash.
:after-backslash-first-line
 - First line following a backslashed continuation.
 - START is the char after the position of the backslash.

:after-block-end
 - Point is after a line containing a block ender.
 - START is the position where the ender starts.
:after-block-start
 - Point is after a line starting a block.
 - START is the position where the block starts.
:after-line
 - Point is after a simple line.
 - START is the position where the previous line starts.
:at-dedenter-block-start
 - Point is on a line starting a dedenter block.
 - START is the position where the dedenter block starts."
  (save-restriction
    (widen)
    (let ((ppss (save-excursion
                  (beginning-of-line)
                  (syntax-ppss))))
      (cond
       ;; Beginning of buffer.
       ((= (line-number-at-pos) 1)
        (cons :no-indent 0))
       ;; Inside a string.
       ((let ((start (nim-syntax-context 'string ppss)))
          (when start
            (cons (if (nim-info-docstring-p)
                      :inside-docstring
                    :inside-string) start))))
       ;; Inside a paren.
       ((let* ((start (nim-syntax-context 'paren ppss))
               (starts-in-newline
                (when start
                  (save-excursion
                    (goto-char start)
                    (forward-char)
                    (not
                     (= (line-number-at-pos)
                        (progn
                          (nim-util-forward-comment)
                          (line-number-at-pos))))))))
          (when start
            (cond
             ;; Current line only holds the closing paren.
             ((save-excursion
                (skip-syntax-forward " ")
                (when (and (nim-syntax-closing-paren-p)
                           (progn
                             (forward-char 1)
                             (not (nim-syntax-context 'paren))))
                  (cons :inside-paren-at-closing-paren start))))
             ;; Current line only holds a closing paren for nested.
             ((save-excursion
                (back-to-indentation)
                (nim-syntax-closing-paren-p))
              (cons :inside-paren-at-closing-nested-paren start))
             ;; This line starts from a opening block in its own line.
             ((save-excursion
                (goto-char start)
                (when (and
                       starts-in-newline
                       (save-excursion
                         (back-to-indentation)
                         (looking-at (nim-rx block-start))))
                  (cons
                   :inside-paren-newline-start-from-block start))))
             (starts-in-newline
              (cons :inside-paren-newline-start start))
             ;; General case.
             (t (cons :inside-paren
                      (save-excursion
                        (goto-char (1+ start))
                        (skip-syntax-forward "(" 1)
                        (skip-syntax-forward " ")
                        (point))))))))
       ;; After backslash.
       ((let ((start (when (not (nim-syntax-comment-or-string-p ppss))
                       (nim-info-line-ends-backslash-p
                        (1- (line-number-at-pos))))))
          (when start
            (cond
             ;; Continuation of dotted expression.
             ((save-excursion
                (back-to-indentation)
                (when (eq (char-after) ?\.)
                  ;; Move point back until it's not inside a paren.
                  (while (prog2
                             (forward-line -1)
                             (and (not (bobp))
                                  (nim-syntax-context 'paren))))
                  (goto-char (line-end-position))
                  (while (and (search-backward
                               "." (line-beginning-position) t)
                              (nim-syntax-context-type)))
                  ;; Ensure previous statement has dot to align with.
                  (when (and (eq (char-after) ?\.)
                             (not (nim-syntax-context-type)))
                    (cons :after-backslash-dotted-continuation (point))))))
             ;; Continuation of block definition.
             ((let ((block-continuation-start
                     (nim-info-block-continuation-line-p)))
                (when block-continuation-start
                  (save-excursion
                    (goto-char block-continuation-start)
                    (re-search-forward
                     (nim-rx block-start (* space))
                     (line-end-position) t)
                    (cons :after-backslash-block-continuation (point))))))
             ;; Continuation of assignment.
             ((let ((assignment-continuation-start
                     (nim-info-assignment-continuation-line-p)))
                (when assignment-continuation-start
                  (save-excursion
                    (goto-char assignment-continuation-start)
                    (cons :after-backslash-assignment-continuation (point))))))
             ;; First line after backslash continuation start.
             ((save-excursion
                (goto-char start)
                (when (or (= (line-number-at-pos) 1)
                          (not (nim-info-beginning-of-backslash
                                (1- (line-number-at-pos)))))
                  (cons :after-backslash-first-line start))))
             ;; General case.
             (t (cons :after-backslash start))))))
       ;; After beginning of block.
       ((let ((start (save-excursion
                       (back-to-indentation)
                       (nim-util-forward-comment -1)
                       (cond ((member (char-before) '(?: ?=))
                              (nim-nav-beginning-of-block))
                             ((re-search-backward (nim-rx decl-block) (line-beginning-position) t)
                              (nim-nav-beginning-of-block))))))
          (when start
            (cons :after-block-start start))))
       ;; At dedenter statement.
       ((let ((start (nim-info-dedenter-statement-p)))
          (when start
            (cons :at-dedenter-block-start start))))
       ;; After normal line, comment or ender (default case).
       ((save-excursion
          (back-to-indentation)
          (skip-chars-backward " \t\n")
          (nim-nav-beginning-of-statement)
          (cons
           (cond ((nim-info-current-line-comment-p)
                  :after-comment)
                 ((save-excursion
                    (goto-char (line-end-position))
                    (nim-util-forward-comment -1)
                    (nim-nav-beginning-of-statement)
                    (looking-at (nim-rx block-ender)))
                  :after-block-end)
                 (t :after-line))
           (point))))))))

(defun nim-indent--calculate-indentation ()
  "Internal implementation of `nim-indent-calculate-indentation'.
May return an integer for the maximum possible indentation at
current context or a list of integers.  The latter case is only
happening for :at-dedenter-block-start context since the
possibilities can be narrowed to specific indentation points."
  (save-restriction
    (widen)
    (save-excursion
      (pcase (nim-indent-context)
        (`(:no-indent . ,_) 0)
        (`(,(or :after-line
                :after-comment
                :inside-string
                :after-backslash
                :inside-paren-at-closing-paren
                :inside-paren-at-closing-nested-paren) . ,start)
         ;; Copy previous indentation.
         (goto-char start)
         (current-indentation))
        (`(:inside-docstring . ,start)
         (let* ((line-indentation (current-indentation))
                (base-indent (progn
                               (goto-char start)
                               (current-indentation))))
           (max line-indentation base-indent)))
        (`(,(or :after-block-start
                :after-backslash-first-line
                :inside-paren-newline-start) . ,start)
         ;; Add one indentation level.
         (goto-char start)
         (+ (current-indentation) nim-indent-offset))
        (`(,(or :inside-paren
                :after-backslash-block-continuation
                :after-backslash-assignment-continuation
                :after-backslash-dotted-continuation) . ,start)
         ;; Use the column given by the context.
         (goto-char start)
         (current-column))
        (`(:after-block-end . ,start)
         ;; Subtract one indentation level.
         (goto-char start)
         (- (current-indentation) nim-indent-offset))
        (`(:at-dedenter-block-start . ,_)
         ;; List all possible indentation levels from opening blocks.
         (let ((opening-block-start-points
                (nim-info-dedenter-opening-block-positions)))
           (if (not opening-block-start-points)
               0  ; if not found default to first column
             (mapcar (lambda (pos)
                       (save-excursion
                         (goto-char pos)
                         (current-indentation)))
                     opening-block-start-points))))
        (`(,(or :inside-paren-newline-start-from-block) . ,start)
         ;; Add two indentation levels to make the suite stand out.
         (goto-char start)
         (+ (current-indentation) (* nim-indent-offset 2)))))))

(defun nim-indent--calculate-levels (indentation)
  "Calculate levels list given INDENTATION.
Argument INDENTATION can either be an integer or a list of
integers.  Levels are returned in ascending order, and in the
case INDENTATION is a list, this order is enforced."
  (if (listp indentation)
      (sort (copy-sequence indentation) #'<)
    (let* ((remainder (% indentation nim-indent-offset))
           (steps (/ (- indentation remainder) nim-indent-offset))
           (levels (mapcar (lambda (step)
                             (* nim-indent-offset step))
                           (number-sequence steps 0 -1))))
      (reverse
       (if (not (zerop remainder))
           (cons indentation levels)
         levels)))))

;; python.el
(defun nim-indent--previous-level (levels indentation)
  "Return previous level from LEVELS relative to INDENTATION."
  (let* ((levels (sort (copy-sequence levels) #'>))
         (default (car levels)))
    (catch 'return
      (dolist (level levels)
        (when (funcall #'< level indentation)
          (throw 'return level)))
      default)))

(defun nim-indent-calculate-indentation (&optional previous)
  "Calculate indentation.
Get indentation of PREVIOUS level when argument is non-nil.
Return the max level of the cycle when indentation reaches the
minimum."
  (let* ((indentation (nim-indent--calculate-indentation))
         (levels (nim-indent--calculate-levels indentation)))
    (if previous
        (nim-indent--previous-level levels (current-indentation))
      (if levels
          (apply #'max levels)
        0))))

(defun nim-indent-line (&optional previous)
  "Internal implementation of `nim-indent-line-function'.
Use the PREVIOUS level when argument is non-nil, otherwise indent
to the maximum available level.  When indentation is the minimum
possible and PREVIOUS is non-nil, cycle back to the maximum
level."
  (let ((follow-indentation-p
         ;; Check if point is within indentation.
         (and (<= (line-beginning-position) (point))
              (>= (+ (line-beginning-position)
                     (current-indentation))
                  (point)))))
    (save-excursion
      (indent-line-to
       (nim-indent-calculate-indentation previous))
      (nim-info-dedenter-opening-block-message))
    (when follow-indentation-p
      (back-to-indentation))))

(defun nim-indent-calculate-levels ()
  "Return possible indentation levels."
  (nim-indent--calculate-levels
   (nim-indent--calculate-indentation)))

(defun nim-indent-line-function ()
  "`indent-line-function' for Nim mode.
When the variable `last-command' is equal to one of the symbols
inside `nim-indent-trigger-commands' it cycles possible
indentation levels from right to left."
  (nim-indent-line
   (and (memq this-command nim-indent-trigger-commands)
        (eq last-command this-command))))

(defun nim-indent-dedent-line ()
  "De-indent current line."
  (interactive "*")
  (when (and (not (bolp))
           (not (nim-syntax-comment-or-string-p))
           (= (current-indentation) (current-column)))
      (nim-indent-line t)
      t))

(defun nim-indent-dedent-line-backspace (arg)
  "De-indent current line.
Argument ARG is passed to `backward-delete-char-untabify' when
point is not in between the indentation."
  (interactive "*p")
  (unless (nim-indent-dedent-line)
    (backward-delete-char-untabify arg)))
(put 'nim-indent-dedent-line-backspace 'delete-selection 'supersede)

(defun nim-indent-region (start end)
  "Indent a nim region automagically.
Called from a program, START and END specify the region to indent."
  (let ((deactivate-mark nil))
    (save-excursion
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (or (bolp) (forward-line 1))
      (while (< (point) end)
        (or (and (bolp) (eolp))
            (when (and
                   ;; Skip if previous line is empty or a comment.
                   (save-excursion
                     (let ((line-is-comment-p
                            (nim-info-current-line-comment-p)))
                       (forward-line -1)
                       (not
                        (or (and (nim-info-current-line-comment-p)
                                 ;; Unless this line is a comment too.
                                 (not line-is-comment-p))
                            (nim-info-current-line-empty-p)))))
                   ;; Don't mess with strings, unless it's the
                   ;; enclosing set of quotes.
                   (or (not (nim-syntax-context 'string))
                       (eq
                        (syntax-after
                         (+ (1- (point))
                            (current-indentation)
                            (nim-syntax-count-quotes (char-after) (point))))
                        (string-to-syntax "|"))))
              (nim-indent-line)))
        (forward-line 1))
      (move-marker end nil))))

(defun nim-indent-shift-left (start end &optional count)
  "Shift lines contained in region START END by COUNT columns to the left.
COUNT defaults to `nim-indent-offset'.  If region isn't
active, the current line is shifted.  The shifted region includes
the lines in which START and END lie.  An error is signaled if
any lines in the region are indented less than COUNT columns."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (if count
      (setq count (prefix-numeric-value count))
    (setq count nim-indent-offset))
  (when (> count 0)
    (let ((deactivate-mark nil))
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (if (and (< (current-indentation) count)
                   (not (looking-at "[ \t]*$")))
              (user-error "Can't shift all lines enough"))
          (forward-line))
        (indent-rigidly start end (- count))))))

(defun nim-indent-shift-right (start end &optional count)
  "Shift lines contained in region START END by COUNT columns to the right.
COUNT defaults to `nim-indent-offset'.  If region isn't
active, the current line is shifted.  The shifted region includes
the lines in which START and END lie."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (let ((deactivate-mark nil))
    (setq count (if count (prefix-numeric-value count)
                  nim-indent-offset))
    (indent-rigidly start end count)))

(defun nim-indent-post-self-insert-function ()
  "Adjust indentation after insertion of some characters.
This function is intended to be added to `post-self-insert-hook.'
If a line renders a paren alone, after adding a char before it,
the line will be re-indented automatically if needed."
  (when (and electric-indent-mode
             (eq (char-before) last-command-event))
    (cond
     ;; Electric indent inside parens
     ((and
       (not (bolp))
       (let ((paren-start (nim-syntax-context 'paren)))
         ;; Check that point is inside parens.
         (when paren-start
           (not
            ;; Filter the case where input is happening in the same
            ;; line where the open paren is.
            (= (line-number-at-pos)
               (line-number-at-pos paren-start)))))
       ;; When content has been added before the closing paren or a
       ;; comma has been inserted, it's ok to do the trick.
       (or
        (memq (char-after) '(?\) ?\] ?\}))
        (eq (char-before) ?,)))
      (save-excursion
        (goto-char (line-beginning-position))
        (let ((indentation (nim-indent-calculate-indentation)))
          (when (and (numberp indentation) (< (current-indentation) indentation))
            (indent-line-to indentation)))))
     ;; Electric colon
     ((and (eq ?: last-command-event)
           (memq ?: electric-indent-chars)
           (not current-prefix-arg)
           ;; Trigger electric colon only at end of line
           (eolp)
           ;; Avoid re-indenting on extra colon
           (not (equal ?: (char-before (1- (point)))))
           (not (nim-syntax-comment-or-string-p)))
      ;; Just re-indent dedenters
      (let ((dedenter-pos (nim-info-dedenter-statement-p))
            (current-pos (point)))
        (when dedenter-pos
          (save-excursion
            (goto-char dedenter-pos)
            (nim-indent-line)
            (unless (= (line-number-at-pos dedenter-pos)
                       (line-number-at-pos current-pos))
              ;; Reindent region if this is a multiline statement
              (nim-indent-region dedenter-pos current-pos)))))))))

(defun nim-indent-electric-colon (arg)
  "Insert a colon and maybe de-indent the current line.
With numeric ARG, just insert that many colons.  With
\\[universal-argument], just insert a single colon."
  (interactive "*P")
  (self-insert-command (if (not (integerp arg)) 1 arg))
  (when (and (not arg)
             (eolp)
             (not (equal ?: (char-after (- (point-marker) 2))))
             (not (nim-syntax-comment-or-string-p)))
    (let ((indentation (current-indentation))
          (calculated-indentation (nim-indent-calculate-indentation)))
      ;(nim-info-closing-block-message)
      (when (> indentation calculated-indentation)
        (save-excursion
          (indent-line-to calculated-indentation)
          ;; (when (not (nim-info-closing-block-message))
          ;;   (indent-line-to indentation)))))))
          )))))
(put 'nim-indent-electric-colon 'delete-selection t)

(provide 'nim-indent)
;;; nim-indent.el ends here
