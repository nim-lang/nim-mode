nim-mode
===========

[![Travis CI](https://travis-ci.org/nim-lang/nim-mode.svg?branch=master)](https://travis-ci.org/nim-lang/nim-mode)

An emacs major mode for the Nim programming language.

* [Install Nim](http://nim-lang.org/download.html)
* Install `nim-mode.el` via melpa.

If you use `company-mode` then add `company-nim` to `company-backends` like:
```el
(add-to-list 'company-backends 'company-nim)
```

If you use `auto-indent-mode` you need to add nim-mode to the list of `auto-indent-multiple-indent-modes`:
```el
(add-to-list 'auto-indent-multiple-indent-modes 'nim-mode)
```

## Commenting
nim-mode refers to `comment-style` variable which comment style user
preferred (whether single line or multi line comment) when user invokes
`comment-region` or `comment-dwim`. See also `comment-styles` variable
for available options.
