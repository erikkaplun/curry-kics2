########################################################################
# Makefile for KiCS2 compiler and REPL
########################################################################

# The kics2 subdirectory for compiled files
SUBDIR   = .curry/kics2
# ghc options
GHC_OPTS = -O2 --make -v1 -cpp -package ghc
# ghc language extensions for compiling translating Curry programs
GHC_EXTS = -XMultiParamTypeClasses -XFlexibleInstances -XRelaxedPolyRec
# ghc includes for compiling translating Curry programs:
GHC_INCL = -i$(ROOT)/runtime:$(ROOT)/runtime/idsupplyghc:$(ROOT)/src/$(SUBDIR)/:$(LIBDIR)/$(SUBDIR)/:$(LIBDIR)/meta/$(SUBDIR)/
# Can be overwritten
GHC      = ghc
GHC_CALL = $(GHC) $(GHC_OPTS) $(GHC_EXTS) $(GHC_INCL)
# bootstrap compiler
KICS2    = $(shell which kics2)
# The kics2c call
KICS2C   = $(COMP) -v2 -i$(LIBDIR):$(LIBDIR)/meta

# Source modules of the compiler (without standard libraries):
COMPILE_DEPS = AbstractHaskell AbstractHaskellGoodies AbstractHaskellPrinter \
               Analysis Base CompilerOpts Dependency2 EliminateCond Files    \
               FlatCurry2AbstractHaskell GetOpt LiftCase Message ModuleDeps  \
               Names SCC SimpleMake Splits State TransFunctions TransTypes   \
               Utils

# Source modules of the REPL (without standard libraries):
REPL_DEPS = AbstractCurryGoodies Files GhciComm Linker Names RCFile Utils

.PHONY: all
all: CompileBoot REPLBoot update_comp

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
#	rm -f ${COMP}.bak ${REPL}.bak
	rm -f *.hi *.o
	@if [ -d $(SUBDIR) ] ; then \
	  cd $(SUBDIR) && rm -f *.hi *.o ; \
	fi

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
	@if [ -x "$(KICS2)" ] ; then \
	  $(KICS2) :l Compile :save :quit ; \
#	  $(MAKE) backup_compiler ; \
	  mv Compile $(COMP) ; \
	else \
	  pakcs -s Compile ; \
#	  ${MAKE} backup_compiler ; \
	  mv Compile.state $(COMP) ; \
	fi

#.PHONY: backup_compiler
#backup_compiler:
#	if [ -f ${COMP} ] ; then mv ${COMP} ${COMP}.bak ; fi

# generate executable for Curry -> Haskell compiler kics2c:
.PHONY: CompileBoot
CompileBoot: CompileBoot.hs $(SUBDIR)/Curry_Compile.hs
	mkdir -p $(LOCALBIN)
	$(GHC_CALL) -o $(COMP) $<
#	${MAKE} backup_compiler

.PHONY: update_comp
update_comp:
	$(KICS2C) $(INSTALLCURRY)
	# recompile compiler with new installation information
	$(MAKE) CompileBoot

# Although $(INSTALLCURRY) is used by the compiler it must not be included
# in the dependencies because for the distribution installation it would
# require a working KICS2 binary *during* installation.
$(SUBDIR)/Curry_Compile.hs: Compile.curry $(COMP_DEPS:=.curry)
	$(KICS2C) $<

########################################################################
# REPL
########################################################################

# generate executable for Curry -> Haskell REPL kics2i:
.PHONY: REPLBoot
REPLBoot: REPLBoot.hs $(SUBDIR)/Curry_REPL.hs
	mkdir -p $(LOCALBIN)
	$(GHC_CALL) -o $(REPL) $<
#	if [ -f ${REPL} ] ; then mv ${REPL} ${REPL}.bak ; fi

$(SUBDIR)/Curry_REPL.hs: REPL.curry $(REPL_DEPS:=.curry) $(INSTALLCURRY)
	$(KICS2C) $<
