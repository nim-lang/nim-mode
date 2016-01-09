# indent between `proc` and `=`
proc test1[T](a: var ref T, finalizer: proc (x: ref T) {.nimcall.})
    {.magic: "NewFinalize", noSideEffect.} =
  echo "hello"


# If proc's return type is `var`
proc `[]`*[A, B](t: var Table[A, B], key: A): var B =
  let opt = getImpl(t, key)


# Check indentation if it's including `var' in the args.
# Second line should indents until "("
proc testVarAsArgs(n: PNode, marker: var IntSet,
                   indent, maxRecDepth: int): Rope


# check after {.'s indent
proc test3[T](a: var ref T, finalizer: proc (x: ref T) {.nimcall.}) {.
  magic: "NewFinalize", noSideEffect.} =
  echo "hello"

proc `[]`*[I: Ordinal;T](a: T; i: I): T {.
  noSideEffect, magic: "ArrGet".}

# check indent after proc signature
type
  emacs_finalizer_function* {.importc: "emacs_finalizer_function",
                              header: "<emacs-module.h>".} = proc(void: pointer)
  emacs_value* {.importc: "struct emacs_value_tag", header: "<emacs-module.h>".} =
    pointer
  other_type = string


# ignore quoted token
proc `type`*(x: expr): typeDesc {.magic: "TypeOf", noSideEffect, compileTime.} =
  discard
