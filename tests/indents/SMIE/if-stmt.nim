var condition = true

proc testIfStmt () =
  if condition:
    echo "hello"
  elif false:
    echo "hello"
  elif false:
    echo "second elif"
  else:
    echo "hello"


if nimvm:
  proc abs*(x: int64): int64 =
    if x < 0: -x else: x
else: # <- check this line is dedented correctly
  proc abs*(x: int64): int64 =
    echo "foo"
