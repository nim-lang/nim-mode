nim-mode
===========

[![Travis CI](https://travis-ci.org/nim-lang/nim-mode.svg?branch=master)](https://travis-ci.org/nim-lang/nim-mode)

An emacs major mode for the Nim programming language. Currently only emacs 25 and higher is supported, if you try to install on latest stable release, you will run into [this problem](https://github.com/nim-lang/nim-mode/issues/100). On arch linux you can install latest version of emacs with `yaourt emacs-git`, but be warned a complete build will need about 45 minutes (also depends on your hardware).

* Install nim either by the [official download](http://nim-lang.org/download.html) or your systems package manager if available.
* Install `nim-mode.el` via [MELPA](https://melpa.org/#/getting-started).
  * `M-x list-packages`  opens the list of all packages (M is the emacs name for Alt)
  * `C-s nim-mode`       moves cursor to nim mode
  * `ESC`                ends search
  * `i`                  tags for install
  * `x`                  executes install
  * `y`                  to confirm question

## Nimsuggest

In nim-mode repository, some *.el files depend on
[nimsuggest](https://github.com/nim-lang/nimsuggest) (not
nim-suggest.el), so if you want to use more integration in Emacs,
please visit the link to install nimsuggest.

Brief descriptions for the nimsuggest related files:
  1. nim-company.el: auto completion feature
  2. nim-thing-at-point.el: thing-at-point for nim
  3. nim-eldoc: show information in minibuffer

Normally it would be enough to install nimsuggest with `nimble install nimsuggest`, but this mode requires that you use the branch `major-restructure`, otherwise you could run into [problem #134](https://github.com/nim-lang/nim-mode/issues/134). So the instructions to build and install are the following:

```bash
git clone git@github.com:nim-lang/nimsuggest.git
cd nimsuggest
git checkout major-restructure
nimble install
```

At this point in time it is untested weather the `major-restructure` branch could break nimsuggest for other editors.

After you install nimsuggest, you may need following configuration.

```el
(setq nim-nimsuggest-path "path/to/nimsuggest")
```

Note that above `nim-nimsuggest-path` variable is automatically set
result of `(executable-find "nimsuggest")`, so if you can get value from
 the `executable-find`, you might don't need above configuration.

## company-mode
If you use `company-mode` then add `company-nim` to `company-backends` like:
```el
(add-to-list 'company-backends
               '(company-nim :with company-nim-builtin))
```

## nim-eldoc
This feature is automatically turned on if `nim-suggest-path` is non-nil.

## auto-indent mode
If you use `auto-indent-mode`, you need to add nim-mode to the list of
`auto-indent-multiple-indent-modes`:
```el
(add-to-list 'auto-indent-multiple-indent-modes 'nim-mode)
```

## Commenting
nim-mode refers to `comment-style` variable which comment style user
preferred (whether single line or multi line comment) when user invokes
`comment-region` or `comment-dwim`. See also `comment-styles` variable
for available options.

## Other convenience packages
- [flycheck-nim](https://github.com/ALSchwalm/flycheck-nim)
- [indent-guide](https://github.com/zk-phi/indent-guide)
- [quickrun](https://github.com/syohex/emacs-quickrun)
