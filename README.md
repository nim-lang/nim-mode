nim-mode
===========

[![Travis CI](https://travis-ci.org/nim-lang/nim-mode.svg?branch=master)](https://travis-ci.org/nim-lang/nim-mode)

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

Normally it would be enough to install nimsuggest with `nimble install nimsuggest`, but currently nim-mode only support specific install way. (Nim 0.14.02 + nimsuggest at 9c8db4b)


```sh
# use nim version v0.14.02
cd /path/to/nim_repository
git checkout tags/0.14.02
git clone --depth 1 https://github.com/nim-lang/csources
cd csources && sh build.sh
cd ..
bin/nim c koch && ./koch boot -d:release

# build nimsuggest
cd /path/to/nimsuggest_repository
git checkout 9c8db4b
nim e compile_without_nimble.nims
```

After you install nimsuggest, you may need following configuration.

```el
(setq nim-nimsuggest-path "path/to/nimsuggest")
```

Note that above `nim-nimsuggest-path` variable is automatically set
result of `(executable-find "nimsuggest")`, so if you can get value from the `executable-find`, you might don't need above configuration.

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
