nim-mode [![Travis CI](https://travis-ci.org/nim-lang/nim-mode.svg?branch=master)](https://travis-ci.org/nim-lang/nim-mode)
===========

A major mode for editing Nim source code

## Note (9/5/2016)
company-mode's configuration (adding company-backends) is no longer
required for auto-completion. capf (completion-at-point-function),
which is Emacs's default feature was added and company-mode also
support this feature as backend for auto-completion.

So, you can remove below configuration if you have:

```lisp
(add-to-list 'company-backends
               '(company-nim :with company-nim-builtin))
```

Also `nimsuggest-mode` was newly added. So if you were already using
nim-mode, please take a look nimsuggest's installing section to
activate `nimsuggest-mode`.

## Prerequisite
- This package requires Emacs 24.4 or higher version.

## Installation

* Install `nim-mode.el` via [MELPA](https://melpa.org/#/getting-started).
  * Check the MELPA's link and add the package archive if you don't set yet
  * `M-x list-packages`  opens the list of all packages (M is the emacs name for Alt)
  * `C-s nim-mode`       moves cursor to nim mode
  * `ESC`                ends search
  * `i`                  tags for install
  * `x`                  executes install
  * `y`                  to confirm question

Please take a look next `Nimsuggest` section if you interested in
editor integration like completion, jump-to-definition, or linting.

## Nimsuggest
(if you are impatient, skip until `install nimsuggest` to install it)

Nimsuggest is an editor agnostic tool for Nim and nim-mode provides:

1. Completion feature -- `C-M-i` and `M-TAB` keys and auto-complete feature if
   you install [company-mode](https://github.com/company-mode/company-mode)
2. Asynchronous linting -- nimsuggest take care .nims files as
   configuration file, so it's smarter than `nim check` command (1)
3. Showing info under the cursor in minibuffer -- (1)
4. Jump to definition feature -- `M-.` for go to def and `M-,` for
   back to before the go to def position

(1): those are automatically turned on if you turned on `nimsuggest-mode`

### Install nimsuggest

1. Use stable version:
   See [official download instruction](http://nim-lang.org/download.html) at
   "Installation based on generated C code" section.

2. Use latest version:
   This way may or may not work (depending on Nim or nimsuggest's
   state and we can't support all the way), so use above way
   if you prefer stable.
   ```sh
   #  assuming you already installed Nim
   cd /path/to/Nim_repository
   ./koch tools
   ```

After you installed nimsuggest, you may need following configuration in
your Emacs configuration file (e.g, ~/.emacs.d/init.el):

```el
;; can be optional.  See below note
(setq nim-nimsuggest-path "path/to/nimsuggest")

;; Currently nimsuggest doesn't support nimscript files, so only nim-mode...
(add-hook 'nim-mode-hook 'nimsuggest-mode)

;; if you installed company-mode (optional)
(add-hook 'nim-mode-hook 'company-mode)
(add-hook 'nimscript-mode-hook 'company-mode)
;; or use below instead if you want to activate `company-mode` on all programming
;; related modes.
;; (add-hook 'prog-mode-hook 'company-mode)
```

Note that above `nim-nimsuggest-path` variable is automatically set
the result of `(executable-find "nimsuggest")`, so if you can get
value from the `executable-find`, you may not need that
configuration unless you want to set specific version of nimsuggest.

## Features
- Syntax highlight for .nim, .nims, nimble, nim.cfg
- Auto-indent
- File outline (`hs-hide-all`, `hs-show-all` etc.)

Not by default, but with nimsuggest and/or other Emacs plugins:

- auto-complete (nimsuggest + company-mode)
- linting on the fly (nimsuggest + flycheck)
- jump-to-definition (nimsuggest)
- refactor (wgrep)

### Other convenience packages for editing Nim source code

Those packages are convenience packages and can be installed same way
as nim-mode (M-x list-packages ...)

- [indent-guide](https://github.com/zk-phi/indent-guide): show visible indent levels
- [quickrun](https://github.com/syohex/emacs-quickrun): emacs port of vim's quickrun
- [company-mode](https://github.com/company-mode/company-mode): auto-complete feature
- [ob-nim](https://github.com/Lompik/ob-nim): org-mode integration focused on Nim
- [wgrep](https://github.com/mhayashi1120/Emacs-wgrep): Writable grep buffer and apply the changes to files (maybe convenient for refactor stuff)
- [suggestion-box-el](https://github.com/yuutayamada/suggestion-box-el): show argument info on the cursor

## auto-indent mode
If you use `auto-indent-mode, you need to add nim-mode to the list of
`auto-indent-multiple-indent-modes`:
```el
(add-to-list 'auto-indent-multiple-indent-modes 'nim-mode)
```

## Other editors/IDEs

You can also find other editor/IDE plugins for Nim language [here](https://github.com/nim-lang/Nim/wiki/editor-support)
