proc testTry2() =
  let x = try:
            parseInt("133a")
          except:
            -1
          finally:
            echo "hi"
