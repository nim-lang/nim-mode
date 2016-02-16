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


type
  RootObj* {.exportc: "TNimObject", inheritable.} =
    object
      s: string # this line should be dedented


type
  A* {.exportc: "TNimObject", inheritable.} =
    object
  B* = ref RootObj # check this line's indent
  C* =
    object # check if there is other "object"


# check indent after pragma
type
  typeA* = proc(): cint {.cdecl.}
  typeB* = proc(): cint {.cdecl.}
  typeC* {.importc: "struct emacs_runtime",
           header: "<emacs-module.h>".} = object
    size: ptrdiff_t
    soemthing: proc(): ptr foo {.cdecl.}
    # check this line's indent
