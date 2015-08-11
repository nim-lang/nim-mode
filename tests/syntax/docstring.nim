# This test checks syntax highlight of docstring (##)

## Top level docstring should be highlighted

proc testInsideProc() =
  ## inside proc's docstring should be highlighted
  echo("foo")

