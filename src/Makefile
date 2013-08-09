########################################################################
# Makefile for KiCS2 compiler and REPL
########################################################################

# The kics2 subdirectory for compiled files
SUBDIR     = .curry/kics2
TYPEINFDIR = $(ROOT)/currytools/typeinference
# ghc options
GHC_OPTS  += -O2 --make -v1 -cpp -package ghc
# ghc language extensions for compiling translated Curry programs
GHC_EXTS   = -XMultiParamTypeClasses -XFlexibleInstances -XRelaxedPolyRec
# ghc includes for compiling translated Curry programs
GHC_INCL   = -i$(ROOT)/runtime:$(ROOT)/runtime/idsupplyghc:$(ROOT)/src/$(SUBDIR):$(LIBDIR)/$(SUBDIR):$(LIBDIR)/meta/$(SUBDIR):$(TYPEINFDIR)/$(SUBDIR)
# Call to ghc binary
GHC_CALL   = "$(GHC)" $(GHC_OPTS) $(GHC_EXTS) $(GHC_INCL)
# The kics2c call
KICS2C     = $(COMP) -v2 -i$(LIBDIR) -i$(LIBDIR)/meta -i$(TYPEINFDIR)
# options for the bootstrapping compiler
CURRY_OPTS = :set path $(TYPEINFDIR):$(LIBDIR)/meta

# Source modules of the compiler (without standard libraries):
COMPILE_DEPS = AbstractHaskell AbstractHaskellGoodies AbstractHaskellPrinter \
               Analysis Base CompilerOpts Dependency2 EliminateCond Files    \
               FlatCurry2AbstractHaskell LiftCase Message ModuleDeps         \
               Names SCC SimpleMake Splits State TransFunctions TransTypes   \
               Utils

# Source modules of the REPL (without standard libraries):
REPL_DEPS = AbstractCurryGoodies Files GhciComm Linker Names RCFile Utils

.PHONY: all
all: CompileBoot updateInstallInfo REPLBoot

.PHONY: bootstrap
bootstrap:
	${MAKE} clean

	# Create kics2 via PAKCS or other KiCS2 (stage 1)
	@echo "Compiling stage 1"
	${MAKE} CompileInitial
	cp ${COMP} ${LOCALBIN}/stage1

	# Create kics2 via kics2 (stage 2)
	@echo "Compiling stage 2"
	$(BINDIR)/cleancurry -r
	${MAKE} CompileBoot
	cp ${COMP} ${LOCALBIN}/stage2

	# Create kics2 via kics2 (stage 3)
	@echo "Compiling stage 3"
	$(BINDIR)/cleancurry -r
	${MAKE} CompileBoot
	cp ${COMP} ${LOCALBIN}/stage3

	@echo "Compiling REPL"
	${MAKE} REPLBoot

# clean all intermediate files of the compiler bootstrapping
.PHONY: clean
clean:
	rm -f $(LOCALBIN)/stage[1,2,3]
	rm -f Compile
	rm -f *.hi *.o
	rm -f $(SUBDIR)/*.hi $(SUBDIR)/*.o

.PHONY: cleanall
cleanall: clean
	rm -rf .curry

########################################################################
# Compile
########################################################################

# generate executable for Curry->Haskell compiler via PAKCS or other KiCS2:
.PHONY: CompileInitial
CompileInitial: Compile.curry
	mkdir -p $(LOCALBIN)
ifeq ($(shell test -x "$(KICS2)" ; echo $$?),0)
	$(KICS2) $(CURRY_OPTS) :l Compile :save :quit
	mv Compile $(COMP)
else
	pakcs $(CURRY_OPTS) :l Compile :save :quit
	mv Compile $(COMP)
endif

# generate executable for Curry -> Haskell compiler kics2c:
.PHONY: CompileBoot
CompileBoot: CompileBoot.hs $(SUBDIR)/Curry_Compile.hs
	mkdir -p $(LOCALBIN)
	$(GHC_CALL) -o $(COMP) $<

# Although $(INSTALLCURRY) is used by the compiler it must not be included
# in the dependencies because for the distribution installation it would
# require a working KICS2 binary *during* installation.
$(SUBDIR)/Curry_Compile.hs: Compile.curry $(COMPILE_DEPS:=.curry) $(COMP)
	$(KICS2C) $<

# recompile installation information and the compiler
.PHONY: updateInstallInfo
updateInstallInfo: $(COMP)
	$(KICS2C) $(INSTALLCURRY)
	$(MAKE) CompileBoot

$(COMP):
	cd .. && $(MAKE) $@

########################################################################
# REPL
########################################################################

# generate executable for Curry -> Haskell REPL kics2i:
.PHONY: REPLBoot
REPLBoot: REPLBoot.hs $(SUBDIR)/Curry_REPL.hs
	mkdir -p $(LOCALBIN)
	$(GHC_CALL) -o $(REPL) $<

$(SUBDIR)/Curry_REPL.hs: REPL.curry $(REPL_DEPS:=.curry) $(INSTALLCURRY) $(COMP)
	$(KICS2C) $<
