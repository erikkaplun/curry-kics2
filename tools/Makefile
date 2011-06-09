# generate various tools for handling Curry programs:

.PHONY: all
all: AddTypes GenInt ImportCalls CreateMakefile
	@cd browser   ; ${MAKE}
	@cd currydoc  ; ${MAKE}
	@cd curry2js  ; ${MAKE}

# generate saved state for addtypes program:
AddTypes: AddTypes.curry
	../bin/kics2 :set v2 :l AddTypes :save :q
	(cd ../bin ; rm -f addtypes ; \
	             ln -s ../tools/AddTypes addtypes)

# generate saved state for interface generator:
GenInt: GenInt.curry
	../bin/kics2 :set v2 :l GenInt :save :q

# generate saved state for ImportCalls tool:
ImportCalls: ImportCalls.curry
	../bin/kics2 :l ImportCalls :save :q

# generate saved state for currycreatemake program:
CreateMakefile: CreateMakefile.curry ../lib/meta/FlatCurryRead.curry
	../bin/kics2 :set v2 :l CreateMakefile :save :q
	(cd ../bin ; rm -f currycreatemake ; \
	             ln -s ../tools/CreateMakefile currycreatemake)

# generate saved state for currytest program:
.PHONY: currytest
currytest: ../bin/currytestkics

../bin/currytestkics: currytest.curry
	/home/pakcs/pakcs/bin/pakcs -s currytest # later replaced by KiCS2
	mv currytest.state ../bin/currytestkics

.PHONY: clean
clean: 
	../bin/cleancurry
	(cd ../bin ; rm -f addtypes currycreatemake currytestkics)
	cd analysis  ; ../../bin/cleancurry
	cd browser   ; ${MAKE} clean
	cd currydoc  ; ${MAKE} clean
	cd curry2js  ; ${MAKE} clean
