var condition = true

proc testCaseStmt () =
  case condition:
    of "with colon":
      echo "if you put ':' after CASE statement, " &
        "next OF operator should be indented."
    of "second OF":
      echo "hello"
    else:
      echo "case done"
