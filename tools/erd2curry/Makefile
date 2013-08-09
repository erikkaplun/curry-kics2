# Makefile for ERD -> Curry compiler

erd2curry: erd2curry.curry \
	  CodeGeneration.curry \
	  Transformation.curry \
	  XML2ERD.curry \
	  ERDGoodies.curry ERD2Graph.curry \
	  ERD.curry
	# create saved state for top-level function "main":
	$(KICS2) ${REPL_OPTS} :set path ../../src :l erd2curry :save :q
	mv erd2curry $(BINDIR)/erd2curry

.PHONY: clean
clean:
	$(CLEANCURRY)
	rm -f $(BINDIR)/erd2curry
