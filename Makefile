update:
	emacs -batch -l test/make-update.el

compile: clean
	emacs -batch -l test/elpa.el -l test/make-compile.el

test:
	emacs -batch -l test/elpa.el -l test/make-test.el

clean:
	rm -f *.elc

.PHONY: update compile test clean
