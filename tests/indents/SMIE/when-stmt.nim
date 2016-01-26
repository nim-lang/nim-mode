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


let x = when true:
          echo "foo"
        elif true:
          echo "bar"
        else:
          echo "buzz"


var y = when true:
          echo "foo"
        elif true:
          echo "bar"
        else:
          echo "buzz"
