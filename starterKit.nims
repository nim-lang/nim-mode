# Just for Emacs beginners to check nim-mode's taste.
# or could be beneficial to check manually something in bare Emacs environment?

# You can execute this file by `nim e starterKit.nims`.

if "" == staticExec("which cask"):
  echo "install cask"
  exec "curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python"

if dirExists(thisDir() & "/.cask"):
  exec "cask install"

const scratchBuffer = """"                                                \
(progn (require 'nim-mode)                                              \
       (nim-mode)                                                       \
       (setq initial-scratch-message                                    \
\"#[                                                                 \\n\
nim-mode's specific keybinds:                                        \\n\
C means Control-key                                                  \\n\
M means Meta-key                                                     \\n\
C-M-a        -- jump to head of proc                                 \\n\
C-M-e        -- jump to end of proc                                  \\n\
C-M-h        -- mark region of function                              \\n\
                                                                     \\n\
after M-x hs-minor-mode                                              \\n\
C-c @ C-M-h  -- hide/fold functions                                  \\n\
C-c @ C-M-s  -- show functions                                       \\n\
]#                                                                   \\n\
proc foo() =                                                         \\n\
  echo 'a' & 'b' & 'c'                                               \\n\
  echo astToStr(bar)                                                 \\n\
                                                                     \\n\
# Note that you can close the current Emacs buffer with C-x C-c.     \\n\
\"))""""

proc startEmacs() =
  exec "cask exec emacs -Q -L . --eval " & scratchBuffer

startEmacs()
