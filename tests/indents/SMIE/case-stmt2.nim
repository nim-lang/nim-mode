var condition = true
proc testCaseStmt2 () =
  case condition
  of "without colon":
    echo "if you don't put ':' after CASE statement, " &
      "next OF operator should not be indented."
  of "second OF":
    echo "hello"
  else:
    echo "case done"
