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


proc testCaseStmt2 () =
  case condition
  of "without colon":
    echo "if you don't put ':' after CASE statement, " &
      "next OF operator should not be indented."
  of "second OF":
    echo "hello"
  else:
    echo "case done"


proc testCaseStmt3 () =
  case condition
  of "without colon":
    echo "if you don't put ':' after CASE statement, " &
      "next OF operator should not be indented."
  of "second OF":
    echo "hello"
  elif true:
    echo "also case statement can include ELIF"
  elif true:
    echo "also case statement can include ELIF"
  else:
    echo "case done"


proc oneline_condition5() =
  case "a"
  of "b": echo "b"
  of "c": echo "c"
  of "d": echo "d"
  else: echo "else"


proc oneline_condition6() =
  case "a":
    of "b": echo "b"
    of "c": echo "c"
    of "d": echo "d"
    else: echo "else"
