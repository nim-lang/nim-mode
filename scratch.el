;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(require 'epc)
(setq epc:debug-out 't)
(setq nim (epc:start-epc "/home/tass/dev/nim/nim/bin/nimsuggest" '("--epc" "/home/tass/dev/nim/nim/compiler/nim.nim")))
(message "Return : %S" (epc:call-sync nim 'sug '("/home/tass/dev/nim/nim/tests/caas/main.nim" 12 7 "/home/tass/dev/nim/nim/tests/caas/main_dirty.nim")))
(switch-to-buffer (process-buffer (epc:manager-server-process nim)))

(message "%S" (epc:sync nim (epc:query-methods-deferred nim)))
(save-excursion
  (switch-to-buffer (get-buffer "main.nim"))
  (goto-line 12)
  (forward-char 5)
  (deferred:$
    (epc:call-deferred
     nim
     'sug
     (list (buffer-file-name)
           (line-number-at-pos)
           (current-column)
           (nim-save-buffer-temporarly)))
    (deferred:nextc it
      (lambda (x) (message x)))))

(with-current-buffer (get-buffer "suggest.nim")
  (message (nim-find-project-main-file)))
