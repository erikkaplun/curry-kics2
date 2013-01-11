# Makefile for building installation utils

utils=pwd cleancurry which

.PHONY: all
all: $(utils)

%: %.hs
	$(GHC) --make -Wall -O2 -o $@ $<

.PHONY: clean
clean:
	rm -f *.hi *.o

.PHONY: cleanall
cleanall: clean
	rm -f $(utils)
