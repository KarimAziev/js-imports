# -*- Makefile -*-

EMACS ?= emacs
CASK ?= cask

BATCHFLAGS = -batch -Q

all:
	${MAKE} clean
	${MAKE} compile
	${MAKE} test
	${MAKE} clean

compile:
	${CASK} exec ${EMACS} -Q --batch -L . --eval "(batch-byte-compile)" js-imports.el

test:
	ret=0 ; \
	outfile=/tmp/.elisp-test-result ; \
	for f in $$(find tests -type f -name "*.el"); do \
	    test -f $$outfile && rm -f $$outfile ; \
		${CASK} exec ${EMACS} $(BATCHFLAGS) -L . -l js-imports.el -l js-imports-test.el \
		-f ert-run-tests-batch-and-exit || ret=1 ; \
	    test -f $$outfile && cat $$outfile ; \
	done ; \
	test $$ret -eq 0

clean:
	rm -f js-imports.elc

.PHONY: all compile test clean