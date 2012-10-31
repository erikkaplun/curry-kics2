# generate various tools for handling Curry programs:

.PHONY: all
all: AddTypes
	@cd curry2js  ; ${MAKE}
	@cd erd2curry ; ${MAKE}

# generate executable for addtypes program:
AddTypes: AddTypes.curry
	../bin/kics2 ${REPL_OPTS} :l AddTypes :save :q
	(cd ../bin ; rm -f addtypes ; \
	             ln -s ../tools/AddTypes addtypes)

.PHONY: clean
clean: 
	../bin/cleancurry
	(cd ../bin ; rm -f addtypes)
	cd curry2js  ; ${MAKE} clean
	cd erd2curry ; ${MAKE} clean
