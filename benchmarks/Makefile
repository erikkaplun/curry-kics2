########################################################################
# Makefile for Benchmarks program
########################################################################

.PHONY: all
all: Benchmarks.state

# generate saved state for Curry->FLVM compiler:
Benchmarks.state: Benchmarks.curry
	cleancurry
	pakcs -s Benchmarks

.PHONY: clean
clean:
	cleancurry
