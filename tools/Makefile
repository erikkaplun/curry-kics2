# generate various tools for handling Curry programs:

.PHONY: all
all: AddTypes GenInt ImportCalls CreateMakefile CurryTest SourceProgGUI
	@cd browser   ; ${MAKE}
	@cd curry2js  ; ${MAKE}
	@cd erd2curry ; ${MAKE}

# generate executable for addtypes program:
AddTypes: AddTypes.curry
	../bin/kics2 ${REPL_OPTS} :l AddTypes :save :q
	(cd ../bin ; rm -f addtypes ; \
	             ln -s ../tools/AddTypes addtypes)

# generate executable for interface generator:
GenInt: GenInt.curry
	../bin/kics2 ${REPL_OPTS} :l GenInt :save :q

# generate executable for ImportCalls tool:
ImportCalls: ImportCalls.curry
	../bin/kics2 :l ImportCalls :save :q

# generate executable for currycreatemake program:
CreateMakefile: CreateMakefile.curry ../lib/meta/FlatCurryRead.curry
	../bin/kics2 ${REPL_OPTS} :l CreateMakefile :save :q
	(cd ../bin ; rm -f currycreatemake ; \
	             ln -s ../tools/CreateMakefile currycreatemake)

# generate executable for currytest program:
CurryTest: CurryTest.curry
	../bin/kics2 ${REPL_OPTS} :l CurryTest :save :q
	(cd ../bin ; rm -f currytest ; \
	             ln -s ../tools/CurryTest currytest)

# generate executable for interface generator:
SourceProgGUI: SourceProgGUI.curry
	../bin/kics2 ${REPL_OPTS} :l SourceProgGUI :save :q

.PHONY: clean
clean: 
	../bin/cleancurry
	(cd ../bin ; rm -f addtypes currycreatemake currytest)
	cd browser   ; ${MAKE} clean
	cd curry2js  ; ${MAKE} clean
	cd erd2curry ; ${MAKE} clean
