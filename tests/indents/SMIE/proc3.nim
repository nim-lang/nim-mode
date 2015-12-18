# check after {.'s indent
proc test3[T](a: var ref T, finalizer: proc (x: ref T) {.nimcall.}) {.
  magic: "NewFinalize", noSideEffect.} =
  echo "hello"
