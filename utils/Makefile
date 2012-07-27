# Makefile for building installation utils

.PHONY: all
all: pwd

pwd: pwd.hs
	ghc --make -Wall -Werror -O2 -o $@ $<

.PHONY: clean
clean:
	rm -f *.hi *.o

.PHONY: cleanall
cleanall: clean
	rm -f pwd
