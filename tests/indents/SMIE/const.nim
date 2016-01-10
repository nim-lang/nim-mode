# single line
const foo: string = "string"
const bar: string = "string"

const
  foo1: string = "string"
  foo2 = "foo"
  bar1: string = "string"
  bar2 = "bar"


# insert newline after equal
const
  foo3: string =
    "string"
  foo4 =
    "foo"


# with pth/ref
when (T is ref):
  const r: ref T
  echo "indent test"
else:
  const p: ptr T
  echo "indent test"


# only type
when nimvm:
  const r: T
  echo "indent test"
else:
  const p: T
  echo "indent test"
