CASK ?= cask
EMACS ?= emacs

all: test

test: clean-elc
	${MAKE} install-dependencies
	${MAKE} unit
	${MAKE} compile
	${MAKE} unit
	${MAKE} clean-elc

compile:
	${CASK} exec ${EMACS} -Q -batch -L . -f batch-byte-compile *.el

unit:
	${CASK} exec buttercup -L .

install-dependencies:
	${CASK} install

clean-elc:
	rm -f *.elc
