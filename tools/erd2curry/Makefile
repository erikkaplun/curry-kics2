# Makefile for ERD->Curry compiler:

LIB=../../lib

.PHONY: all
all: erd2curry

erd2curry: erd2curry.curry \
	  CodeGeneration.curry \
	  Transformation.curry \
	  XML2ERD.curry \
	  ERDGoodies.curry ERD2Graph.curry \
	  ERD.curry
	# create saved state for top-level function "main":
	../../bin/kics2 ${REPL_OPTS} :set path ../../src :l erd2curry :save :q
	(cd ../../bin ; rm -f erd2curry \
	              ; ln -s ../tools/erd2curry/erd2curry)

.PHONY: clean
clean:
	../../bin/cleancurry
	rm -f ../../bin/erd2curry
