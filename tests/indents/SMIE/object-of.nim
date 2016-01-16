type
  Unit = object of Thing
    x: int
    y: int
  NewType* = object of RootEffect # check this line is dedented

type
  A =
    object of RootRef
  B[T] =
    object of WrappedItem
      value: T


# with ref
type
  C =
    ref object of RootRef
  D[T] =
    ref object of WrappedItem
      value: T


type
  Node = ref NodeObj
  NodeObj {.acyclic, final.} = object
    left, right: Node
    data: string
    height: int
    weight: int
  NewTyp = string

type
  RootEffect* {.compilerproc.} = object of RootObj
  TimeEffect* = object of RootEffect   ## Time effect.
  IOEffect* = object of RootEffect     ## IO effect.
  ReadIOEffect* = object of IOEffect   ## Effect describing a read IO operation.
  WriteIOEffect* = object of IOEffect  ## Effect describing a write IO operation.
  ExecIOEffect* = object of IOEffect   ## Effect describing an executing IO operation.
  Exception* {.compilerproc.} = object of RootObj
    trace: string
  SystemError* = object of Exception
  IOError* = object of SystemError
