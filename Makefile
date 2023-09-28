EASK ?= eask
EMACS ?= emacs

all: test

# TODO: Add `test` when it's stable!
ci: build compile

build:
	${EASK} package
	${EASK} install

test:
	${EASK} install-deps --dev
	${MAKE} compile
	${MAKE} unit
	${MAKE} clean-elc

compile:
	${EASK} compile

unit:
	${EASK} exec buttercup -L .

clean-elc:
	${EASK} clean elc
