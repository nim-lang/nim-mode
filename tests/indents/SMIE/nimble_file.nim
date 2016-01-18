# Package

version       = "0.1.0"
author        = "Jhon Do"
description   = "Test"
license       = "XXX"

# Dependencies

requires "nim >= 0.12.1"

task tests, "test next line's indent":
  exec "this line should be indented correctly"
