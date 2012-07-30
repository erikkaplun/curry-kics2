# Makefile for building installation utils

utils=pwd cabaldir cleancurry

.PHONY: all
all: $(utils)

%: %.hs
	ghc --make -Wall -Werror -O2 -o $@ $<

.PHONY: clean
clean:
	rm -f *.hi *.o

.PHONY: cleanall
cleanall: clean
	rm -f $(utils)
