proc testFor =
  for x, y in foo:
    echo "process"
    if true:
      echo "break keep indent"
      break
    else:
      echo "foo"


proc testFor =
  for x, y in foo:
    if x == true: break
    else: break
