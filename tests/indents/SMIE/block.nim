# Those tests are mainly checking after `break`
proc testblock1 =
  block myblock1:
    echo("entering block")
    break
  echo "leave block"

proc testBlock2() =
  block myBlockName:
    echo "foo"
    echo("bar")
    break myBlockName
  echo "foo"

proc testBlock3() =
  block myblock2:
    echo("entering block")
    while true:
      echo("looping")
      break # leaves the loop, but not the block
    echo("still in block")
    break
  echo "exit from myblock2"

proc testBlock4 =
  while true:
    var x = nil
    if x.isNil: break
  echo "should be dedented after break"

proc testBlock5 =
  while true:
    echo "process"
    if true:
      echo "something"
      break
    echo "after break should be dedented"
