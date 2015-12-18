# check indent after proc signature
type
  emacs_finalizer_function* {.importc: "emacs_finalizer_function",
                              header: "<emacs-module.h>".} = proc(void: pointer)
  emacs_value* {.importc: "struct emacs_value_tag", header: "<emacs-module.h>".} = pointer
