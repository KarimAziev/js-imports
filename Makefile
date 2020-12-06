# -*- Makefile -*-

EMACS ?= emacs
CASK ?= cask

BATCHFLAGS = -batch -Q

all:
	${MAKE} clean
	${MAKE} test
	${MAKE} compile
	${MAKE} test
	${MAKE} clean

compile:
	${CASK} exec ${EMACS} -Q --batch -L . --eval "(batch-byte-compile)" js-import.el

test:
	ret=0 ; \
	outfile=/tmp/.elisp-test-result ; \
	for f in $$(find tests -type f -name "*.el"); do \
	    test -f $$outfile && rm -f $$outfile ; \
		${CASK} exec ${EMACS} $(BATCHFLAGS) -L . -l js-import.el -l tests/js-import-test.el \
		-f ert-run-tests-batch-and-exit || ret=1 ; \
	    test -f $$outfile && cat $$outfile ; \
	done ; \
	test $$ret -eq 0

clean:
	rm -f js-import.elc

.PHONY: all compile test clean