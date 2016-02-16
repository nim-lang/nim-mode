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


# check `var` and `else`
var a = when true: "a"
        else: "b"
echo "check this line's indent"

# check `let` and `else`
let b = when true: "a"
        else: "b"
echo "check this line's indent"
