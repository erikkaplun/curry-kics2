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

# binary directory and executables
BINDIR=../bin
COMP=${BINDIR}/idc
REPL=${BINDIR}/idci

.PHONY: all
all: CompileBoot REPLBoot

.PHONY: bootstrap
bootstrap:
	${MAKE} clean
	# Create kics2 via PAKCS (stage 1)
	@echo "Compiling stage 1"
	${MAKE} Compile.state
	cp ${COMP} ../stage1
	# Create kics2 via kics2 (stage 2)
	@echo "Compiling stage 2"
	${MAKE} CompileBoot
	cp ${COMP} ../stage2
	# Create kics2 via kics2 (stage 3)
	cd .. ; bin/cleancurry -r
	@echo "Compiling stage 3"
	${MAKE} CompileBoot
	cp ${COMP} ../stage3
	@echo "Compiling REPL"
	${MAKE} REPLBoot

########################################################################
# Compile
########################################################################

# generate executable for Curry->Haskell compiler via PAKCS:
Compile.state: ${INSTALLCURRY} Compile.curry
	pakcs -s Compile
	mv -b Compile.state ${COMP}

# generate executable for Curry->Haskell compiler:
CompileBoot: .curry/kics2/Curry_Compile.hs CompileBoot.hs
	ghc ${GHCOPTIONS} ${GHCEXTS} ${GHCINCLUDES} CompileBoot.hs
	mv -b CompileBoot ${COMP}

# Translate Curry->Haskell compiler into Haskell:
.curry/kics2/Curry_Compile.hs: ${COMPILERSOURCES} ${INSTALLCURRY}
	${COMP} -v2 -i../lib:../lib/meta Compile.curry

########################################################################
# REPL
########################################################################

# generate executable for Curry->Haskell REPL:
REPLBoot: .curry/kics2/Curry_REPL.hs REPLBoot.hs
	ghc ${GHCOPTIONS} ${GHCEXTS} ${GHCINCLUDES} REPLBoot.hs
	mv -b REPLBoot ${REPL}

# Translate Curry->Haskell compiler into Haskell:
.curry/kics2/Curry_REPL.hs: ${REPLSOURCES} ${INSTALLCURRY}
	${COMP} -v2 -i../lib:../lib/meta REPL.curry

########################################################################
# clean
########################################################################

# clean all intermediate files of the compiler bootstrapping
.PHONY: clean
clean:
	cd .. ; rm -f stage1 stage2 stage3
	rm -f ${COMP}~ ${REPL}~
	rm -f *.hi *.o
	cd ../runtime; rm -f *.hi *.o *.hi-boot *.o-boot
	rm -f ../runtime/idsupply*/*.hi ../runtime/idsupply*/*.o
