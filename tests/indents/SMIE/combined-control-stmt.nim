var condition = true

proc complex() =
  case condition:
    of "with colon":
      if true:
        echo "foo"
      else: echo "one line else"
    of "second OF":
      echo "hello"
    of "one line OF": echo "hello"
    else:
      echo "case done"
      when true:
        echo "hello"
      elif true:
        echo "make sure 'elif' is working"
      else: echo "one line else"
      if true:
        echo "after ELSE ended with single line"
