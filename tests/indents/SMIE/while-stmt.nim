var condition = true

proc testWhile () =
  while condition:
    condition = false
    break
  echo "indent end with break"


proc testWhile2 =
  while true:
    var x = nil
    if x.isNil: break
    echo "should be dedented after break"


proc testWhile3 =
  while true:
    echo "process"
    if true:
      echo "something"
      break
    echo "after break should be dedented"
