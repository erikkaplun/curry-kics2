# generate various tools for handling Curry programs:

.PHONY: all
all: GenInt ImportCalls

# generate saved state for interface generator:
GenInt: GenInt.curry
	../bin/kics2 :l GenInt :save :q

# generate saved state for ImportCalls tool:
ImportCalls: ImportCalls.curry
	../bin/kics2 :l ImportCalls :save :q

# generate saved state for currytest program:
.PHONY: currytest
currytest: ../bin/currytestkics

../bin/currytestkics: currytest.curry
	/home/pakcs/pakcs/bin/pakcs -s currytest # later replaced by KiCS2
	mv currytest.state ../bin/currytestkics

.PHONY: clean
clean: 
	../bin/cleancurry
	rm -f ../bin/currytestkics
