# Just for Emacs beginners to check nim-mode's taste.
# or could be beneficial to check manually something in bare Emacs environment?

# You can execute this file by `nim e starterKit.nims`.

import strutils, ospaths

const caskInstallCommand =
  "curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go" &
    "| python"

if "" == findExe("cask"):
  echo "Install Cask (a package manager for Emacs)"
  exec caskInstallCommand

if dirExists(thisDir() / ".cask"):
  exec "cask install"

const nimCode = """#[
nim-mode's specific keybinds:
#############################

C means Control-key
M means Meta-key
C-M-a        -- jump to head of proc
C-M-e        -- jump to end of proc
C-M-h        -- mark region of function

After M-x hs-minor-mode
#######################

C-c @ C-M-h  -- hide/fold functions
C-c @ C-M-s  -- show functions

Nimsuggest related commands
###########################
(this needs extra configuration. See also README.md)

M-.          --  jump to definition
]#
proc foo() =
  echo 'a' & 'b' & 'c'
  echo astToStr(bar)

# Note that you can close the current Emacs buffer with C-x C-c.
""".split(NewLines).join("\\n")

const emacsConfig = """"
(progn
  (unless (version<= \"24.4\" emacs-version)
    (error \"nim-mode needs emacs version 24.4 or later\"))
  (require 'nim-mode)
  (nim-mode)
  (setq initial-scratch-message $#))
"""".split(NewLines).join(" ")

const scratchBuffer = emacsConfig.format("\\\"" & nimCode & "\\\"")

proc startEmacs() =
  exec "cask exec emacs -Q -L . --eval " & scratchBuffer

startEmacs()
