iterator a1(ticker: int) {.closure.} =
  echo "a1: A"
  yield
  echo "a1: B"
  yield
  echo "a1: C"
  yield
  echo "a1: D"
