# indent between `proc` and `=`
proc test1[T](a: var ref T, finalizer: proc (x: ref T) {.nimcall.})
    {.magic: "NewFinalize", noSideEffect.} =
  echo "hello"
