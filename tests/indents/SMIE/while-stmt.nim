var condition = true

proc testWhileStmt () =
  while condition:
    condition = false
    break
  echo "indent end with break"
