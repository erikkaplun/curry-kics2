########################################################################
# Makefile for ID compiler
########################################################################

# The Curry installation info
INSTALLCURRY=Installation.curry
# ghc options
GHCOPTIONS=-O2 --make -v1
# ghc language extensions for compiling translating Curry programs:
GHCEXTS=-XMultiParamTypeClasses -XFlexibleInstances -XRelaxedPolyRec
# ghc includes for compiling translating Curry programs:
GHCINCLUDES=-i../runtime:../runtime/idsupplyinteger:./.curry/kics2/:../lib/.curry/kics2/:../lib/meta/.curry/kics2/

# Source modules of the compiler (without standard libraries):
COMPILERSOURCES = Compile.curry \
	          AbstractHaskellPrinter.curry \
	          FlatCurry2Types.curry \
	          LiftCase.curry \
	          Message.curry ModuleDeps.curry \
	          SimpleMake.curry Splits.curry \
	          Dependency2.curry GetOpt.curry \
	          AbstractHaskellGoodies.curry FlatCurry2AbstractHaskell.curry \
	          Names.curry Analysis.curry \
	          FiniteMap.curry \
	          SCC.curry Base.curry Files.curry \
	          AbstractHaskell.curry \
	          CompilerOpts.curry Utils.curry

# Source modules of the REPL (without standard libraries):
REPLSOURCES = REPL.curry RCFile.curry Files.curry Names.curry

.PHONY: all
all: CompileBoot REPLBoot

.PHONY: bootstrap
bootstrap:
	${MAKE} clean
	# Create kics2 via PAKCS (stage 1)
	echo "Compiling stage 1"
	${MAKE} Compile.state
	cp ../idc ../stage1
	# Create kics2 via kics2 (stage 2)
	echo "Compiling stage 2"
	${MAKE} CompileBoot
	cp ../idc ../stage2
	# Create kics2 via kics2 (stage 3)
	${MAKE} clean
	echo "Compiling stage 3"
	${MAKE} CompileBoot
	cp ../idc ../stage3
	echo "Compiling REPL"
	${MAKE} REPL

########################################################################
# Compile
########################################################################

# generate executable for Curry->Haskell compiler via PAKCS:
Compile.state: ${INSTALLCURRY} Compile.curry
	pakcs -s Compile
	-mv ../idc ../idc.bak
	cp -p Compile.state ../idc

# generate executable for Curry->Haskell compiler:
CompileBoot: .curry/kics2/Curry_Compile.hs CompileBoot.hs
	ghc ${GHCOPTIONS} ${GHCEXTS} ${GHCINCLUDES} CompileBoot.hs
	-mv ../idc ../idc.bak
	cp -p CompileBoot ../idc

# Translate Curry->Haskell compiler into Haskell:
.curry/kics2/Curry_Compile.hs: ${COMPILERSOURCES} ${INSTALLCURRY}
	../idc -v2 -i../lib:../lib/meta Compile.curry

########################################################################
# REPL
########################################################################

# generate executable for Curry->Haskell REPL:
REPLBoot: .curry/kics2/Curry_REPL.hs REPLBoot.hs
	ghc ${GHCOPTIONS} ${GHCEXTS} ${GHCINCLUDES} REPLBoot.hs
	-mv ../REPLexec ../REPLexec.bak
	cp -p REPLBoot ../REPLexec

# Translate Curry->Haskell compiler into Haskell:
.curry/kics2/Curry_REPL.hs: ${REPLSOURCES} ${INSTALLCURRY}
	../idc -v2 -i../lib:../lib/meta REPL.curry

########################################################################
# clean
########################################################################

# clean all intermediate files of the compiler bootstrapping
.PHONY: clean
clean:
	rm -f CompileBoot Compile.state REPLBoot
	rm -f *.hi *.o
	cd ../runtime; rm -f *.hi *.o *.hi-boot *.o-boot
	rm -f ../runtime/idsupply*/*.hi ../runtime/idsupply*/*.o
