proc oneline_condition1() =
  if true: echo "foo"
  echo "foo"


proc oneline_condition2() =
  when true: echo "foo"
  echo "foo"


proc oneline_condition3() =
  if true:
    echo "foo"
  elif true: echo "foo"
  echo "foo"


proc oneline_condition4() =
  if true: echo "foo"
  else: echo "foo"
  echo "foo"


proc oneline_condition5() =
  case "a"
  of "b": echo "b"
  echo "foo"
