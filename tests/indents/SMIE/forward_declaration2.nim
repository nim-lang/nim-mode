# Test forward declaration after pragma
proc testForwardDeclaration() =
  proc getOccupiedMem*(): int {.rtl.}
  proc getFreeMem*(): int {.rtl.}
  proc getOccupiedSharedMem*(): int {.rtl.}
  echo("check after forward declaration")
