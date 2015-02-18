nim-mode
===========

An emacs major mode for the Nim programming language.

* [Install Nim](http://nim-lang.org/download.html)
* Install `nim-mode.el` via elpa.

If you use `auto-complete` for completion, see `ac-nim.el`
for info about how to enable a nim-specific source (experimental).

If you use `auto-indent-mode` you need to add nim-mode to the list of `auto-indent-multiple-indent-modes`:
```el
(add-to-list 'auto-indent-multiple-indent-modes 'nim-mode)
```