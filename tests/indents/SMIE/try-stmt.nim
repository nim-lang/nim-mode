proc testTryStmt() =
  var f: File
  if open(f, "file"):
    try:
      var a = readLine(f)
      echo("num:" & $(parseInt(a)))
    except OverflowError:
      echo("overflow")
    except Others:
      echo("something")
    finally:
      close(f)


proc testTry2() =
  let x = try:
            parseInt("133a")
          except:
            -1
          finally:
            echo "hi"
