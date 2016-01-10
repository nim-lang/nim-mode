# single line
var foo: string = "string"
var bar: string = "string"

var
  foo1: string = "string"
  foo2 = "foo"
  bar1: string = "string"
  bar2 = "bar"


# insert newline after equal
var
  foo3: string =
    "string"
  foo4 =
    "foo"


# with pth/ref
when (T is ref):
  var r: ref T
  echo "indent test"
else:
  var p: ptr T
  echo "indent test"


# only type
when nimvm:
  var r: T
  echo "indent test"
else:
  var p: T
  echo "indent test"
