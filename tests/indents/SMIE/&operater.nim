proc testAnd =
  echo "foo" &
    "continuous &" &
    "after end of &"
  echo "dedent this line"

proc testAndWithComment =
  echo "foo" &            # comment
    "continuous &" &      # comment
    "after end of &"      # comment
  echo "dedent this line" # comment

proc testAnd2 =
  echo "foo" & "bar" & "inside &"
  echo "foo"
