type
  emacs_env* {.importc: "struct emacs_env_25", header: "<emacs-module.h>".} = object
    vec_size*: proc(env: ptr emacs_env, vec: emacs_value): ptrdiff_t
    otherObj : string


type
  Callback = proc (s: string):string {.raises: [IOError].}
  otherType = int
