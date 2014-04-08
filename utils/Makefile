# Makefile for building installation utils

utils = $(foreach u, pwd cleancurry which, $(u)$(EXE_SUFFIX))

.PHONY: all
all: $(utils)

%$(EXE_SUFFIX): %.hs
	"$(GHC)" --make -Wall -O2 -o $@ $<

.PHONY: clean
clean:
	rm -f *.hi *.o

.PHONY: cleanall
cleanall: clean
	rm -f $(utils)
