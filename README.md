`nim-mode` A major mode for editing Nim source code
===================================================
[![Travis CI](https://travis-ci.org/nim-lang/nim-mode.svg?branch=master)](https://travis-ci.org/nim-lang/nim-mode)
[![MELPA](http://melpa.org/packages/nim-mode-badge.svg)](http://melpa.org/#/nim-mode)
[![MELPA Stable](http://stable.melpa.org/packages/nim-mode-badge.svg)](http://stable.melpa.org/#/nim-mode)

This package provides (and requires Emacs 24.4 or higher version):

- Syntax highlighting for ``*.nim``, ``*.nims``, ``*.nimble`` and
  ``nim.cfg`` files
- `nim-compile` command (*C-c C-c*), with error matcher for the
  compile buffer
- Nimsuggest (alpha):
  - on the fly linter using flycheck, or flymake (from Emacs 26)
  - auto-completion with company-mode ("C-M-i" for manual completion)
  - jump-to-definition (*M-.*, and *M-,* keys)
  - find-references (*M-?* key)
  - eldoc or help on hover in term of LSP
- Automatic indentation and line breaking (alpha)
- Outline by procedures (`hs-hide-all`, `hs-show-all` etc.)

## Installation

* ensure packages from [MELPA](https://melpa.org/#/getting-started)
  can be installed
* Install `nim-mode` (e.g. ``M-x package-install RET nim-mode RET``)

## nimsuggest (alpha)

At the time of writing this it should be mentioned that both
nimsuggest and nimsuggest-mode have problems that could cause emacs to
be much less responsive, or even freeze.  Apart from that is is non
trivial to configure nimsuggest with the right parameters so that you
also get correct results. So you have been warned.

Nimsuggest is the compilation server for Nim, it runs in its own
process, pretty much independent of emacs. ``nimsuggest-mode``
is an emacs minor mode that comes with ``nim-mode``.  It is
responsible to create the nimsuggest instance and connect emacs with it.
``nimsuggest-mode`` doesn't do anything visual in emacs yet. There are
other minor modes such as ``flycheck``,``flymake`` (linting) and
``company`` (completion) that are responsible for editor integration.

- ``flycheck`` and ``flymake`` are two alternative linting
  engines. Before emacs version ``26.1`` ``flymake`` was pretty much
  outdated and the recommended linting engine was the external
  ``flycheck``.  But from version ``26.1`` onward, ``flymake`` is a
  good linting engine that comes with emacs.  But you should not use both
  at the same time.
- ``flycheck-nimsuggest`` is a backend for flycheck. It builds the bridge
  to ``nimsuggest-mode`` so that flycheck can visualize the linting
  information that nimsuggest provides.
- ``flycheck-nim`` is an alternative backend for flycheck that does
  not interact with nimsuggest at all. Instead it uses the ``nim
  check`` command and parses the output of that command.
- ``flymake-nimsuggest`` is the backend for ``flymake`` to build the
  bridge to ``nimsuggest-mode``. It comes with ``nim-mode``, and it is
  activated automatically in nim files, when ``flymake-mode`` is on.
- ``company-mode`` is a minor mode for auto completion (company - complete anything)
- ``company-nimsuggest`` is the backend for ``company-mode`` that
  builds the bridge to ``nimsuggest-mode``.
- ``eldoc-mode`` is a minor mode for emacs that is responsible to show
  the documentation of emacs lisp symbols at point, hence the name.
  But ``eldoc-mode`` has been extended to work for
  other programming languages as well.  ``nimsuggest-mode`` has
  integration for ``eldoc-mode`` so you can see documentation of nim
  symbols at point when ``nimsuggest-mode`` is active.

For ``nimsuggest-mode`` to work, emacs needs to be able to find the nimsuggest binary, when it
is on the path, it should just work, if not you can customize
``nimsuggest-path``.  Since it is completely optional to use
``nimsuggest``, you have to activate ``nimsuggest-mode`` manually.

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

### Keyboard Shortcuts (with nimsuggest)

1.  Completion feature -- *C-M-i* and *M-TAB*  keys and auto-complete feature if
   you install [company-mode](https://github.com/company-mode/company-mode)
2. Jump to Definition -- *M-.* to find the definition for symbol at
   poisition and *M-,* to go back.
3. Show Doc -- *C-c C-d* Show documentation of symbol at current
   position in the dedicated `*nim-doc*` buffer.
4. Show Short Doc -- (automatically) Shows the short documentation of the symbol at
   point in the minibuffer

## Grammar and Indentation

In ``nim-smie.el`` there are nim grammar rules for ``smie``
(Simple Minded Indentation Engine).  These rules give emacs a basic
understanding of the Nim grammar.  They are used to calculate a "correct"
indentation level for code, and to fill (distribute line endings at
margin) comments, multiline strings and other parts of the code.

``electric-indent-mode``, a global minor mode that is turned on by
default, uses the rules from ``nim-smie.el`` to automatically reindent
the current line, before a new line is started on *RET*.  The rules
sometimes can really make the emacs behave sluggish up to freezing for
several seconds.  The problem is most noticeable when the grammar gets
confused with incomplete statements or the grammar becomes very
uncommon through the usage of untyped macros for embedded domain
language.  Just as an example writing patterns for the nim library
``ast-pattern-matching`` really confuses smie and you might have to
manually fix a lot of indentation that ``electric-indent-mode`` breaks
automatically.

My recommendation is to turn off electric indentation for Nim
files.  This can be done locally with
``(electric-indent-local-mode 0)``, or globally (not just Nim files) with
``(electric-indent-mode 0)``.  Nim has semantic whitespace,  therefore
it might be better if the indentation is something that is inserted manually.

``auto-fill-mode``, a minor mode, uses the rules to break lines
automatically.  At the moment it is also not recommend to enable
``auto-fill-mode`` for Nim files.  But using `fill-paragraph` (*M-q*) on
comments does work reliably and it is very useful.

## Example Configuration

You can copy and adjust the following configuration into your local
`init.el` file.

```elisp
;; The `nimsuggest-path' will be set to the value of
;; (executable-find "nimsuggest"), automatically.
(setq nimsuggest-path "path/to/nimsuggest")

(defun my--init-nim-mode ()
  "Local init function for `nim-mode'."

  ;; Just an example, by default these functions are
  ;; already mapped to "C-c <" and "C-c >".
  (local-set-key (kbd "M->") 'nim-indent-shift-right)
  (local-set-key (kbd "M-<") 'nim-indent-shift-left)

  ;; Make files in the nimble folder read only by default.
  ;; This can prevent to edit them by accident.
  (when (string-match "/\.nimble/" (or (buffer-file-name) "")) (read-only-mode 1))

  ;; If you want to experiment, you can enable the following modes by
  ;; uncommenting their line.
  ;; (nimsuggest-mode 1)
  ;; Remember: Only enable either `flycheck-mode' or `flymake-mode' at the same time.
  ;; (flycheck-mode 1)
  ;; (flymake-mode 1)

  ;; The following modes are disabled for Nim files just for the case
  ;; that they are enabled globally.
  ;; Anything that is based on smie can cause problems.
  (auto-fill-mode 0)
  (electric-indent-local-mode 0)
)

(add-hook 'nim-mode-hook 'my--init-nim-mode)

```


## Other convenience packages for editing Nim source code

Those packages are convenience packages and can be installed same way
as nim-mode (M-x list-packages ...)

- [indent-guide](https://github.com/zk-phi/indent-guide): show visible indent levels
- [quickrun](https://github.com/syohex/emacs-quickrun): emacs port of vim's quickrun
- [company-mode](https://github.com/company-mode/company-mode): auto-complete feature
- [ob-nim](https://github.com/Lompik/ob-nim): org-mode integration focused on Nim
- [wgrep](https://github.com/mhayashi1120/Emacs-wgrep): Writable grep buffer and apply the changes to files (maybe convenient for refactor stuff)
- [suggestion-box-el](https://github.com/yuutayamada/suggestion-box-el): show argument info on the cursor

## Other editors/IDEs

You can also find other editor/IDE plugins for
Nim language [here](https://github.com/nim-lang/Nim/wiki/editor-support)
