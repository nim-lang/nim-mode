var condition = true

proc testWhenStmt () =
  when condition:
    echo "hello"
  elif false:
    echo "hello"
  elif false:
    echo "second elif"
  else:
    echo "hello"
