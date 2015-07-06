case thing:
of "string":
  echo("foo")
of 100:
  echo("number")
  if true:
    echo("wrong indent")
  else:
    echo("foo")
of nil:
  echo("foo")
