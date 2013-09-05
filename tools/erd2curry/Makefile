# Makefile for ERD -> Curry compiler

ERD2CURRY=$(BINDIR)/erd2curry

$(ERD2CURRY): erd2curry.curry \
	  CodeGeneration.curry \
	  Transformation.curry \
	  XML2ERD.curry \
	  ERDGoodies.curry ERD2Graph.curry \
	  ERD.curry
	# create saved state for top-level function "main":
	$(REPL) $(REPL_OPTS) :set path $(ROOT)/src :l erd2curry :save :q
	mv erd2curry $@

.PHONY: clean
clean:
	$(CLEANCURRY)
	rm -f $(ERD2CURRY)
