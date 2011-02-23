########################################################################
# Makefile for ID compiler improved
########################################################################

.PHONY: all
all: idc
	chmod -R go+rX .

# generate saved state for Curry->FLVM compiler:
idc: Compile.curry FlatCurry2AbstractHaskell.curry FlatCurry2Types.curry \
	         Names.curry AbstractHaskell.curry \
	         AbstractHaskellGoodies.curry AbstractHaskellPrinter.curry
	pakcs -s Compile && mv Compile.state idc

.PHONY: clean
clean:
	cleancurry
	rm -f idc
	rm -f *.hi *.o
	rm -f ./examples/Curry_*.hs
