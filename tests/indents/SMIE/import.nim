import
  parseutils, strutils, parseopt, parsecfg, strtabs, unicode, pegs, ropes,
  os, osproc, times


# new syntax from version 0.16.0
import compiler / [ast,
                   parser,
                   lexer]

import compiler / [
  bitsets,
  cgen,
  depends
]


