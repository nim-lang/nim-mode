# single line
let foo: string = "string"
let bar: string = "string"

let
  foo1: string = "string"
  foo2 = "foo"
  bar1: string = "string"
  bar2 = "bar"


# insert newline after equal
let
  foo3: string =
    "string"
  foo4 =
    "foo"


# with pth/ref
when (T is ref):
  let r: ref T
  echo "indent test"
else:
  let p: ptr T
  echo "indent test"


# only type
when nimvm:
  let r: T
  echo "indent test"
else:
  let p: T
  echo "indent test"
