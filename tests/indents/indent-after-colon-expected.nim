# This test checks indentation after ":"

# defer statement
defer:
  echo("should indent after colon")

defer: # comment's colon :
  echo("should indent above line has comment")

# Do notation
sort(cities) do (x,y: string) -> int:
  cmp(x.len, y.len)

cities = cities.map do (x:string) -> string:
  "City of " & x

# Template
template withFile(f, fn, mode: expr, actions: stmt): stmt {.immediate.} =
  block:
    var f: File  # since 'f' is a template param, it's injected implicitly

withFile(txt, "test.txt", fmWrite):
  txt.writeln("should indent after colon")

# static statement
static:
  echo "should indent after colon"

# string
discard "End of colon should be ignored :
This line should not indented.
"
