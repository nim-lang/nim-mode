# Test highlight for NimScript files
mode = ScriptMode.Verbose

when true:
  task build, "<- check highlight":
    echo "foo"
elif true:
  task tests, "<- check highlight":
    echo "foo"
elif true:
  task bench, "<- check highlight":
    echo "foo"
else:
  exec "echo foo"
