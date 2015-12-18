proc testBlock() =
  block myblock2:
    echo("entering block")
    while true:
      echo("looping")
      break # leaves the loop, but not the block
    echo("still in block")
    break
  echo "exit from myblock2"
