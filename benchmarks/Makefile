########################################################################
# Makefile for Benchmarks program
########################################################################

.PHONY: all
all: Benchmarks.state

# generate saved state for Curry->FLVM compiler:
Benchmarks.state: Benchmarks.curry
	cleancurry
	mv Prelude.curry Prelude.curry.ID
	pakcs -s Benchmarks
	mv Prelude.curry.ID Prelude.curry

.PHONY: clean
clean:
	cleancurry
