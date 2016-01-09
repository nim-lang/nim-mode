type
  intmax_t* {.importc: "intmax_t", header: "<inttypes.h>".} =
    clonglong
  emacs_finalizer_function* {.importc: "emacs_finalizer_function",
                              header: "<emacs-module.h>".} = proc(void: pointer)
  emacs_value* {.importc: "struct emacs_value_tag",
                 header: "<emacs-module.h>".} = pointer
  ptrdiff_t* {.importc: "ptrdiff_t", header: "<stddef.h>".} = int
  emacs_env* {.importc: "struct emacs_env_25",
               header: "<emacs-module.h>".} = object
    size: ptrdiff_t
    make_global_ref*: proc(env: ptr emacs_env, any_reference: emacs_value): emacs_value
    free_global_ref*: proc(env: ptr emacs_env, global_reference: emacs_value): pointer
