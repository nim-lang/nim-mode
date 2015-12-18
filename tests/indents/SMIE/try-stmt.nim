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
