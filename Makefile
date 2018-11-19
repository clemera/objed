update:
	emacs -Q -batch -l test/make-update.el

compile: clean
	emacs -Q -batch -l test/elpa.el -l test/make-compile.el

test: compile
	emacs -Q -batch -l test/elpa.el -l test/make-test.el

clean:
	rm -f *.elc

.PHONY: update compile test clean
