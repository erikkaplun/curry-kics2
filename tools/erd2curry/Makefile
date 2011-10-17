# Makefile for ERD->Curry compiler:

LIB=../../lib

.PHONY: all
all: Main

Main: Main.curry \
	  CodeGeneration.curry \
	  Transformation.curry \
	  XML2ERD.curry \
	  ERDGoodies.curry ERD2Graph.curry \
	  ERD.curry
	# create saved state for top-level function "main":
	../../bin/kics2 :set path ../../src :l Main :save :q
	(cd ../../bin ; rm -f erd2curry \
	              ; ln -s ../tools/erd2curry/Main erd2curry)

.PHONY: clean
clean:
	../../bin/cleancurry
	rm -f ../../bin/erd2curry
