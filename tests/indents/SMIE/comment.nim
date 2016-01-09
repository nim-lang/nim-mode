# comment test after empty line

proc foo =
  # comment after `=`
  echo "check this line's indent"
  echo "hello" # comment
               # after previous line's comment

  var a:string = "a"
  # comment


# After comment
let
  a = 100
  b = 200


# multiple comment lines
# comment ...
let
  c = 100
  d = 200


type
  SomeSignedInt* = int|int8|int16|int32|int64
    ## docgen comment
  otherType = string # <- check this line's indent

type
  RootEffect* {.compilerproc.} = object of RootObj ## \
    ## comment after \\
