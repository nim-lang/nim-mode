# check highlight of `varargs`
proc testVarargs[T](x: varargs[T]): T =
  result = x[0]
  for i in 1..high(x):
    if x[i] < result: result = x[i]
