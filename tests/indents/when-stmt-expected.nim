const title = "This test checks whether the `when` statement is indented."

# at top level
when true:
  echo "after `when` statement should be indented."
elif not true:
  echo "after `elif` statement should be indented."
else:
  echo "after `else` statement should be indented."

# with proc statement
proc procWhenStmt() =
  when false:
    echo "after `when` statement should be indented."
  elif true:
    echo "after `elif` statement should be indented."
  else:
    echo "after `else` statement should be indented."

# Check if's `else` statement after when's `else`
proc whenWithIfElseStmt() =
  if false:
    when false:
      echo "after `when` statement should be indented."
    elif true:
      echo "after `elif` statement should be indented."
    else:
      echo "after `else` statement should be indented."
  else:
    echo "if's `else` statement should be dedented."

# Check if's `elif` statement after when's `else`
proc whenWithIfElifStmt() =
  if false:
    when false:
      echo "after `when` statement should be indented."
    elif true:
      echo "after `elif` statement should be indented."
    else:
      echo "after `else` statement should be indented."
  elif:
    echo "if when statement is already closed by `else`, should be dedented."

# Check case's `else` statement after when's `else`
proc whenWithCaseElifStmt() =
  case title
  of "foo":
    when false:
      echo "after `when` statement should be indented."
    elif true:
      echo "after `elif` statement should be indented."
    else:
      echo "after `else` statement should be indented."
  else:
    echo "if when statement is already closed by `else`, should be dedented."

# Check when's `else` statement after if's `else`
proc whenWithCaseElifStmt() =
  when false:
    echo "after `when` statement should be indented."
  elif true:
    if true:
      echo "foo"
    else:
      echo "bar"
  else:
    echo "after `else` statement should be dedented."

# Check when's `else` statement after case's `else`
proc whenWithCaseElifStmt() =
  when false:
    echo "after `when` statement should be indented."
  elif true:
    case title
    of:   echo "foo"
    else: echo "bar"
  else:
    echo "after `else` statement should be dedented."

# Check when's `else` whether it doesn't affect if's `else` statement
proc whenWithCaseElifStmt() =
  when false:
    echo "foo"
  elif true:
    if true: echo "bar"
    elif: echo "This `elif` should be indented at `if` statement level."
    else: echo "baz"
  else:
    echo "after `else` statement should be dedented."

# Check when's `else` whether it doesn't affect case's `else` statement
proc whenWithCaseElifStmt() =
  when false:
    echo "foo"
  elif true:
    case title
    of:   echo "bar"
    elif true: echo "this `elif` should be indented at case's indent level."
    else: echo "baz"
  else:
    echo "after `else` statement should be dedented."
