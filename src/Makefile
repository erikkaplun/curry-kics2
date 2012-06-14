########################################################################
# Makefile for ID compiler
########################################################################

# The absolute path of the installation directory:
IDIR=`pwd | sed 's|/src$$||'`

# The Curry installation info
INSTALLCURRY=Installation.curry
# ghc options
GHCOPTIONS=-O2 --make -v1 -cpp
# ghc language extensions for compiling translating Curry programs:
GHCEXTS=-XMultiParamTypeClasses -XFlexibleInstances -XRelaxedPolyRec -package ghc
# ghc includes for compiling translating Curry programs:
GHCINCLUDES=-i${IDIR}/runtime:${IDIR}/runtime/idsupplyghc:${IDIR}/src/.curry/kics2/:${IDIR}/lib/.curry/kics2/:${IDIR}/lib/meta/.curry/kics2/

# Source modules of the compiler (without standard libraries):
COMPILERSOURCES = Compile.curry \
	          AbstractHaskellPrinter.curry \
	          TransTypes.curry TransFunctions.curry \
	          LiftCase.curry \
                  State.curry EliminateCond.curry \
	          Message.curry ModuleDeps.curry \
	          SimpleMake.curry Splits.curry \
	          Dependency2.curry GetOpt.curry \
	          AbstractHaskellGoodies.curry FlatCurry2AbstractHaskell.curry \
	          Names.curry Analysis.curry \
	          SCC.curry Base.curry Files.curry \
	          AbstractHaskell.curry \
	          CompilerOpts.curry Utils.curry

# Source modules of the REPL (without standard libraries):
REPLSOURCES = REPL.curry RCFile.curry Files.curry Names.curry


# binary directory and executables
BINDIR=${IDIR}/bin
LOCALBIN=${BINDIR}/.local
COMP=${LOCALBIN}/kics2c
REPL=${LOCALBIN}/kics2i

.PHONY: all
all: CompileBoot REPLBoot

.PHONY: bootstrap
bootstrap:
	${MAKE} clean
	# Create kics2 via PAKCS or other KiCS2 (stage 1)
	@echo "Compiling stage 1"
	${MAKE} CompileInitial
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

# generate executable for Curry->Haskell compiler via PAKCS or other KiCS2:
.PHONY: CompileInitial
CompileInitial: ${INSTALLCURRY} Compile.curry
	mkdir -p ${LOCALBIN}
	@if [ -n "${KICS2}" ] ; then \
	  ${KICS2} :l Compile :save :quit ; \
	  ${MAKE} backup_compiler ; \
	  mv Compile ${COMP} ; \
	else \
	  pakcs -s Compile ; \
	  ${MAKE} backup_compiler ; \
	  mv Compile.state ${COMP} ; \
	fi

.PHONY: backup_compiler
backup_compiler:
	if [ -f ${COMP} ] ; then mv ${COMP} ${COMP}.bak ; fi

# generate executable for Curry->Haskell compiler:
CompileBoot: .curry/kics2/Curry_Compile.hs CompileBoot.hs
	ghc ${GHCOPTIONS} ${GHCEXTS} ${GHCINCLUDES} CompileBoot.hs
	${MAKE} backup_compiler
	mkdir -p ${LOCALBIN}
	mv CompileBoot ${COMP}

# Translate Curry->Haskell compiler into Haskell:
.curry/kics2/Curry_Compile.hs: ${COMPILERSOURCES}
	${COMP} -v2 -i${IDIR}/lib:${IDIR}/lib/meta Compile.curry

########################################################################
# REPL
########################################################################

# generate executable for Curry->Haskell REPL:
REPLBoot: .curry/kics2/Curry_REPL.hs \
	  .curry/kics2/Curry_Installation.hs REPLBoot.hs
	ghc ${GHCOPTIONS} ${GHCEXTS} ${GHCINCLUDES} REPLBoot.hs
	if [ -f ${REPL} ] ; then mv ${REPL} ${REPL}.bak ; fi
	mkdir -p ${LOCALBIN}
	mv REPLBoot ${REPL}

# Translate Curry->Haskell compiler into Haskell:
.curry/kics2/Curry_REPL.hs: ${REPLSOURCES}
	${COMP} -v2 -i${IDIR}/lib:${IDIR}/lib/meta REPL.curry

# Translate Curry->Haskell compiler into Haskell:
.curry/kics2/Curry_Installation.hs: Installation.curry
	${COMP} -v2 -i${IDIR}/lib Installation.curry
	# recompile compiler with new installation information:
	rm ${COMP}
	${MAKE} CompileBoot

########################################################################
# clean
########################################################################

# clean all intermediate files of the compiler bootstrapping
.PHONY: clean
clean:
	cd .. ; rm -f stage1 stage2 stage3
	rm -f ${COMP}~ ${REPL}~
	rm -f *.hi *.o
	@if [ -d .curry/kics2 ] ; then cd .curry/kics2 && rm -f *.hi *.o ; fi
	cd ../runtime; rm -f *.hi *.o *.hi-boot *.o-boot
	rm -f ../runtime/idsupply*/*.hi ../runtime/idsupply*/*.o
