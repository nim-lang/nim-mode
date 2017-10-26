Running Tests
=============

You'll need [cask](https://github.com/cask/cask). Then run

    cask install

for first time setup. For running the tests, use

    cask exec buttercup -L .

Writing Tests
=============

The tests are written with
[buttercup](https://github.com/jorgenschaefer/emacs-buttercup/) Take a
look at the existing tests. Currently, code samples for testing are in
the `tests/samples/` subdirectory. The naming convention is
`something-actual.nim` and `something-expected.nim`.

Making New Issues
=================

Most of `nim-mode`s features depend on nimsuggest. If you think your issue is
related to nimsuggest, as a first debug step, see if the problem disappears when
you disable nimsuggest.

You can turn off nimsuggest by setting `nimsuggest-path` to `nil`:

```lisp
;; place this configuration in your .emacs or somewhere before emacs
;; load nim-mode and you may need to reboot your Emacs to check.
(defconst nimsuggest-path nil)
```

`nim-mode` uses nimsuggest via `company-mode`, `eldoc-mode`,
`flycheck-mode`, and `nimsuggest-find-definition` (goto definition).

If you are completely new to Emacs, please check next section as well.

Emacs Configuration or Debugging
================================

Some configuration variables are placed in nim-vars.el. Please take a look if
you are interested. They should be available via customize-group. (indenting,
faces, nimsuggest, etc.)

If you new to Emacs, please visit [here](https://github.com/chrisdone/elisp-guide)
and check `Evaluation` section (or other stuff). The C-x C-e or C-M-x
is really convenient way to debug if you get errors in your configuration file.
