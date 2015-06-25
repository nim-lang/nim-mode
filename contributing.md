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
