# make EMACS=$HOME/emacs/<version>/bin/emacs
EMACS = emacs

update:
	$(EMACS) -Q -batch -l test/make-update.el

compile: clean
	$(EMACS) -Q -batch -l test/elpa.el -l test/make-compile.el

test: compile
	$(EMACS) -Q -batch -l test/elpa.el -l test/make-test.el

clean:
	rm -f *.elc

.PHONY: update compile test clean
