proc testAfterBreak =
  while true:
    echo "process"
    if true:
      echo "something"
      break
    echo "after break should be dedented"
