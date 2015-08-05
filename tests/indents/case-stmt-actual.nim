var
  afterColon = "after colon"
  withoutColon = "without colon"


case afterColon:
    of "after colon":
    echo("should indent after case statement")
    of nil:
    echo("check indent of second `of`")
    elif true:
    echo("check indent of `elif`")
    else:
    echo("check indent else")

case withoutColon
    of "without colon after":
    echo("should dedent after case statement")
    of nil:
    echo("check indent of second `of`")
    elif true:
    echo("check indent of `elif`")
    else:
    echo("check indent else")


case afterColon:
    of "after colon": echo("should indent after case statement")
    of nil: echo("check indent of second `of`")
    elif true: echo("check indent of `elif`")
    else: echo("check indent else")

case withoutColon
    of "without colon after": echo("should dedent after case statement")
    of nil: echo("check indent of second `of`")
    else: echo("check indent else")


proc insideProc() =
  case afterColon:
      of "after colon":
      echo("should indent after case statement")
      of nil:
      echo("check indent of second `of`")
      elif true:
      echo("check indent of `elif`")
      else:
      echo("check indent else")

proc insideProcWithoutColon() =
     case withoutColon
     of "without colon after":
     echo("should dedent after case statement")
     of nil:
     echo("check indent of second `of`")
     elif true:
     echo("check indent of `elif`")
     else:
     echo("check indent else")

proc oneLineAfterColon() =
     case afterColon:
     of "after colon": echo("should indent after case statement")
     of nil: echo("check indent of second `of`")
     elif true: echo("check indent of `elif`")
     else: echo("check indent else")

proc oneLineWithoutColon() =
     case withoutColon
     of "without colon after": echo("should dedent after case statement")
     of nil: echo("check indent of second `of`")
     elif true: echo("check indent of `elif`")
     else: echo("check indent else")


proc distinguishElseOfIf(arg: int) =
     case arg:
     of 100:
     if true: echo("foo")
     else:
     echo("foo")
     else:
     discard "should distinguish indent after if's else"


proc oneLineDistinguishElseOfIf(arg: int) =
  case arg:
  of 100:
  if true: echo("foo")
  else: echo("foo")
  else: discard "should distinguish indent after if's else"
