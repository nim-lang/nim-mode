# check after break's behavior with setting variable:
#   (setq nim-smie-dedent-after-break '("if" "when" "elif" "else" "finally"))
proc breakTest() =
  block:
    if true: echo "else break"
    else: break
  echo "`else` with `break` should be dedented"

proc breakTest2() =
  while true:
    if true: echo "else break"
    elif true: break
  echo "`else` with `break` should be dedented"

proc testFor =
  for x, y in foo:
    if x == true: break
    else: break
  echo "after else break should be dedented"

proc testTryWithBreak() =
  block:
    try: parseInt("133a")
    except: -1
    finally: break
  echo "after `finally: break` should be dedented"
