# Makefile for building installation utils

.PHONY: all
all: pwd cabaldir

pwd: pwd.hs
	ghc --make -Wall -Werror -O2 -o $@ $<

cabaldir: cabaldir.hs
	ghc --make -Wall -Werror -O2 -o $@ $<

.PHONY: clean
clean:
	rm -f *.hi *.o

.PHONY: cleanall
cleanall: clean
	rm -f pwd cabaldir
