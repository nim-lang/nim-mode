proc testFor =
  for x, y in foo:
    echo "process"
    if true:
      echo "break keep indent"
      break
    else:
      echo "foo"
