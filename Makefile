CASK ?= cask
EMACS ?= emacs

cask_dir_exists := $(shell find ./ -type d -name ".cask")

all: test

test: clean-elc
	$(if $(cask_dir_exists), $(skip), ${MAKE} install-dependencies)
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

define skip
	@echo ".cask already exists, dependecies' installation skipped."
	@echo "If you want to update those dependencies, execute 'make install-dependencies'."
endef
