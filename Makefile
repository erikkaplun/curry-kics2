########################################################################
# Makefile for KiCS2 compiler suite
########################################################################

# Some information about this installation
# ----------------------------------------

# Is this a global installation (with restricted flexibility) (yes/no)?
GLOBALINSTALL   = yes
# Should profiling be enabled (yes/no)?
PROFILING       = yes
# The major version number
MAJORVERSION    = 0
# The minor version number
MINORVERSION    = 3
# The revision version number
REVISIONVERSION = 3
# Complete version
export VERSION  = $(MAJORVERSION).$(MINORVERSION).$(REVISIONVERSION)
# The version date, extracted from the last git commit
COMPILERDATE   := $(shell git log -1 --format="%ci" | cut -c-10)
# The installation date, set to the current date
INSTALLDATE    := $(shell date)
# The name of the Curry system, needed for installation of currytools
export CURRYSYSTEM = kics2
# Windows operating system?
ifneq (,$(findstring MINGW, $(shell uname)))
export WINDOWS    = 1
export EXE_SUFFIX = .exe
else
export EXE_SUFFIX =
endif

# Paths used in this installation
# -------------------------------

# root directory of the installation
export ROOT          = $(CURDIR)
# binary directory and executables
export BINDIR        = $(ROOT)/bin
# Directory where the libraries are located
export LIBDIR        = $(ROOT)/lib
# Directory where the libraries are located
export DOCDIR        = $(ROOT)/docs
# Directory where local executables are stored
export LOCALBIN      = $(BINDIR)/.local
# installation prefix, may be overwritten
export INSTALLPREFIX = $(ROOT)
# Directory where local package installations are stored
export LOCALPKG      = $(INSTALLPREFIX)/pkg
# The path to the package database
export PKGDB         = $(LOCALPKG)/kics2.conf.d

# Special files and binaries used in this installation
# ----------------------------------------------------

# The compiler binary
export COMP         = $(LOCALBIN)/kics2c$(EXE_SUFFIX)
# The REPL binary, used for building the libraries
export REPL         = $(LOCALBIN)/kics2i$(EXE_SUFFIX)
# The default options for the REPL, used for libraries and tools
export REPL_OPTS    = :set v2 :set -ghci
# The standard name of the interactive Curry system in then bin dirctory:
export CURRYSYSTEMBIN = $(BINDIR)/curry
# The frontend binary
export CYMAKE       = $(BINDIR)/cymake$(EXE_SUFFIX)
# The cleancurry binary
export CLEANCURRY   = $(BINDIR)/cleancurry$(EXE_SUFFIX)
# The currydoc binary
export CURRYDOC     = $(BINDIR)/currydoc$(EXE_SUFFIX)
# The Haskell installation info
export INSTALLHS    = $(ROOT)/runtime/Installation.hs
# The Curry installation info
export INSTALLCURRY = $(ROOT)/src/Installation.curry
# The version information for the manual
MANUALVERSION       = $(ROOT)/docs/src/version.tex
# Logfiles for make
MAKELOG             = make.log
# Utility programs
PWD                 = utils/pwd$(EXE_SUFFIX)
WHICH               = utils/which$(EXE_SUFFIX)

# Cabal packages on which this installation depends
# -------------------------------------------------

# Dependencies for the kics2 runtime system
export RUNTIMEDEPS = base containers ghc mtl parallel-tree-search tree-monad
# Dependencies for the kics2 libraries
export LIBDEPS     = base directory network old-time parallel-tree-search \
                     process time
# Dependency to system library
ifdef WINDOWS
export SYSTEMDEPS  = Win32
else
export SYSTEMDEPS  = unix
endif
# All dependencies. Note that "sort" also removes duplicates.
export ALLDEPS     = $(sort $(RUNTIMEDEPS) $(LIBDEPS) $(SYSTEMDEPS))

# GHC and CABAL configuration
# ---------------------------

# The path to the Glasgow Haskell Compiler and Cabal
export GHC     := $(shell which ghc)
export GHC-PKG := $(shell dirname "$(GHC)")/ghc-pkg
export CABAL    = cabal

# Because of an API change in GHC 7.6,
# we need to distinguish GHC < 7.6 and GHC >= 7.6.
# GHC 7.6 renamed the option "package-conf" to "package-db".

# extract GHC version
GHC_MAJOR := $(shell "$(GHC)" --numeric-version | cut -d. -f1)
GHC_MINOR := $(shell "$(GHC)" --numeric-version | cut -d. -f2)
# Is the GHC version >= 7.6 ?
GHC_GEQ_76 = $(shell test $(GHC_MAJOR) -gt 7 -o \( $(GHC_MAJOR) -eq 7 \
              -a $(GHC_MINOR) -ge 6 \) ; echo $$?)
# package-db (>= 7.6) or package-conf (< 7.6)?
ifeq ($(GHC_GEQ_76),0)
GHC_PKG_OPT = package-db
else
GHC_PKG_OPT = package-conf
endif

# Libraries installed with GHC
GHC_LIBS := $(shell "$(GHC-PKG)" list --global --simple-output --names-only)
# Packages used by the compiler
GHC_PKGS  = $(foreach pkg,$(ALLDEPS),-package $(pkg))

# Standard options for compiling target programs with ghc.
# Uses our own package db and explicitly exposes the packages
# to avoid conflicts with globally installed ones.
export GHC_OPTS       = -no-user-$(GHC_PKG_OPT) -$(GHC_PKG_OPT) "$(PKGDB)" \
                        -hide-all-packages $(GHC_PKGS)
# Command to unregister a package
export GHC_UNREGISTER = "$(GHC-PKG)" unregister --$(GHC_PKG_OPT)="$(PKGDB)"
# Command to install missing packages using cabal
export CABAL_INSTALL  = "$(CABAL)" install --with-compiler="$(GHC)"       \
                        --with-hc-pkg="$(GHC-PKG)" --prefix="$(LOCALPKG)" \
                        --global --package-db="$(PKGDB)" -O2
# Cabal profiling options
ifeq ($(PROFILING),yes)
export CABAL_PROFILE = -p
else
export CABAL_PROFILE  =
endif
# Additional flags passed to the runtime
export RUNTIMEFLAGS   =

########################################################################
# The targets
########################################################################

# main (default) target - starts installation with logging
.PHONY: all
all:
	@rm -f ${MAKELOG}
	@echo "Make started at `date`" > ${MAKELOG}
	$(MAKE) install 2>&1 | tee -a ${MAKELOG}
	@echo "Make finished at `date`" >> ${MAKELOG}
	@echo "Make process logged in file ${MAKELOG}"

# install the complete system
.PHONY: install
install: kernel tools manual
	chmod -R go+rX .

# remove files from user's home directory
.PHONY: uninstall
uninstall:
	rm -rf $(HOME)/.kics2rc $(HOME)/.kics2rc.bak $(HOME)/.kics2i_history
	@echo "Just remove this directory to finish uninstallation."

# install additional tools
.PHONY: tools
tools:
	cd currytools && $(MAKE) # shared tools
	cd tools      && $(MAKE) # compiler specific tools
	cd cpns       && $(MAKE) # Curry Port Name Server demon
	cd www        && $(MAKE) # scripts for dynamic web pages

# install the kernel system (binaries and libraries)
.PHONY: kernel
kernel: $(PWD) $(WHICH) $(PKGDB) $(CYMAKE) $(CLEANCURRY) scripts copylibs
	$(MAKE) $(INSTALLCURRY) INSTALLPREFIX="$(shell $(PWD))" \
	                        GHC="$(shell $(WHICH) "$(GHC)")"
	cd src     && $(MAKE) # build compiler
	rm -f $(CURRYSYSTEMBIN)
	ln -s $(BINDIR)/$(CURRYSYSTEM) $(CURRYSYSTEMBIN)
ifeq ($(GLOBALINSTALL),yes)
	cd lib     && $(MAKE) unregister
	cd runtime && $(MAKE) unregister
	cd runtime && $(MAKE)
	cd lib     && $(MAKE)
endif

# install the library sources from the trunk directory:
.PHONY: copylibs
copylibs:
	@if [ -d lib-trunk ] ; then cd lib-trunk && $(MAKE) -f Makefile.$(CURRYSYSTEM).install ; fi

# create package database
$(PKGDB):
	"$(GHC-PKG)" init $@
	$(CABAL) update
	$(CABAL_INSTALL) $(CABAL_PROFILE) $(filter-out $(GHC_LIBS),$(ALLDEPS))

# create frontend binary
$(CYMAKE): .FORCE
	cd frontend && $(MAKE)

.PHONY: scripts
scripts: $(PWD)
	cd scripts && $(MAKE) ROOT=$(shell $(PWD))

$(CLEANCURRY): utils/cleancurry$(EXE_SUFFIX)
	mkdir -p $(@D)
	cp $< $@

# build installation utils
utils/%: .FORCE
	cd utils && $(MAKE) $(@F)

# run the test suite to check the installation
.PHONY: runtest
runtest: testsuite/doTest
	cd testsuite && ./doTest --nogui

.PHONY: clean
clean: $(CLEANCURRY)
	-cd benchmarks && $(MAKE) clean
	cd cpns        && $(MAKE) clean
	cd currytools  && $(MAKE) clean
	-cd docs/src   && $(MAKE) clean
	cd frontend    && $(MAKE) clean
	-cd lib        && $(MAKE) clean
	cd runtime     && $(MAKE) clean
	cd src         && $(MAKE) clean
	-cd talks      && $(MAKE) clean
	cd tools       && $(MAKE) clean
	cd utils       && $(MAKE) clean
	cd www         && $(MAKE) clean
	rm -f $(MAKELOG) $(CURRYSYSTEMBIN)
	rm -f $(INSTALLHS) $(INSTALLCURRY)

# clean everything (including compiler binaries)
.PHONY: cleanall
cleanall: clean
	-cd docs/src && $(MAKE) cleanall
	-cd lib      && $(MAKE) cleanall
	cd scripts   && $(MAKE) cleanall
	cd src       && $(MAKE) cleanall
	-cd talks    && $(MAKE) cleanall
	cd utils     && $(MAKE) cleanall
	rm -rf $(LOCALBIN) $(CYMAKE) $(LOCALPKG)
	rm -f  $(CLEANCURRY)

.PHONY: maintainer-clean
maintainer-clean: cleanall
	rm -rf $(BINDIR)
	rm -rf $(LIBDIR)

.PHONY: .FORCE
.FORCE:

##############################################################################
# Building the compiler itself
##############################################################################

# generate module with basic installation information
$(INSTALLCURRY): $(INSTALLHS)
	cp $< $@

$(INSTALLHS): Makefile
ifneq ($(shell test -x "$(GHC)" ; echo $$?), 0)
	$(error "Executable 'ghc' not found. You may use 'make <target> GHC=<path>')
endif
	echo "-- This file is automatically generated, do not change it!" > $@
	echo "module Installation where" >> $@
	echo "" >> $@
	echo 'compilerName :: String' >> $@
	echo 'compilerName = "KiCS2 Curry -> Haskell Compiler"' >> $@
	echo "" >> $@
	echo 'installDir :: String' >> $@
	echo 'installDir = "$(INSTALLPREFIX)"' >> $@
	echo "" >> $@
	echo 'majorVersion :: Int' >> $@
	echo 'majorVersion = $(MAJORVERSION)' >> $@
	echo "" >> $@
	echo 'minorVersion :: Int' >> $@
	echo 'minorVersion = $(MINORVERSION)' >> $@
	echo "" >> $@
	echo 'revisionVersion :: Int' >> $@
	echo 'revisionVersion = $(REVISIONVERSION)' >> $@
	echo "" >> $@
	echo 'compilerDate :: String' >> $@
	echo 'compilerDate = "$(COMPILERDATE)"' >> $@
	echo "" >> $@
	echo 'installDate :: String' >> $@
	echo 'installDate = "$(INSTALLDATE)"' >> $@
	echo "" >> $@
	echo 'runtime :: String' >> $@
	echo 'runtime = "ghc"' >> $@
	echo "" >> $@
	echo 'runtimeMajor :: Int' >> $@
	echo 'runtimeMajor = $(GHC_MAJOR)' >> $@
	echo "" >> $@
	echo 'runtimeMinor :: Int' >> $@
	echo 'runtimeMinor = $(GHC_MINOR)' >> $@
	echo "" >> $@
	echo 'ghcExec :: String' >> $@
	echo 'ghcExec = "\"$(GHC)\""' >> $@
	echo "" >> $@
	echo 'ghcOptions :: String' >> $@
ifeq ($(GLOBALINSTALL),yes)
	echo 'ghcOptions = "$(subst ",\",$(GHC_OPTS)) -package kics2-runtime -package kics2-libraries -package kics2-libraries-trace"' >> $@
else
	echo 'ghcOptions = "$(subst ",\",$(GHC_OPTS))"' >> $@
endif
	echo "" >> $@
	echo 'installGlobal :: Bool' >> $@
ifeq ($(GLOBALINSTALL),yes)
	echo 'installGlobal = True' >> $@
else
	echo 'installGlobal = False' >> $@
endif
	echo "" >> $@
	echo 'withProfiling :: Bool' >> $@
ifeq ($(PROFILING),yes)
	echo 'withProfiling = True' >> $@
else
	echo 'withProfiling = False' >> $@
endif

##############################################################################
# Create documentation for system libraries:
##############################################################################

.PHONY: libdoc
libdoc: $(CURRYDOC)
	@rm -f ${MAKELOG}
	@echo "Make libdoc started at `date`" > ${MAKELOG}
	@cd lib && $(MAKE) doc 2>&1 | tee -a ../${MAKELOG}
	@echo "Make libdoc finished at `date`" >> ${MAKELOG}
	@echo "Make libdoc process logged in file ${MAKELOG}"

.PHONY: currydoc
currydoc: $(CURRYDOC)

$(CURRYDOC):
	cd currytools && $(MAKE) currydoc

##############################################################################
# Create the KiCS2 manual
##############################################################################

MANUAL = docs/Manual.pdf

$(MANUAL):
	$(MAKE) manual

.PHONY: manual
manual:
	$(MAKE) $(CURRYDOC)
	# generate manual, if necessary:
	@if [ -d docs/src ] ; then \
	  $(MAKE) ${MANUALVERSION} && cd docs/src && $(MAKE) install ; \
	fi

${MANUALVERSION}: Makefile
	echo '\\newcommand{\\kicsversiondate}'         >  $@
	echo '{Version $(VERSION) of ${COMPILERDATE}}' >> $@

.PHONY: cleanmanual
cleanmanual:
	-cd docs/src && $(MAKE) clean

# SNIP FOR DISTRIBUTION - DO NOT REMOVE THIS COMMENT

##############################################################################
# Distribution targets
##############################################################################

# temporary directory to create distribution version
TMP      = /tmp
FULLNAME = kics2-$(VERSION)
TMPDIR   = $(TMP)/$(FULLNAME)
TARBALL  = $(FULLNAME).tar.gz

# generate a source distribution of KiCS2
.PHONY: dist
dist:
	# remove old distribution
	rm -f $(TARBALL)
	$(MAKE) $(TARBALL)

# publish the distribution files in the local web pages
HTMLDIR = ${HOME}/public_html/kics2/download
.PHONY: publish
publish: $(TARBALL)
	cp $(TARBALL) docs/INSTALL.html ${HTMLDIR}
	chmod -R go+rX ${HTMLDIR}
	@echo "Don't forget to run 'update-kics2' to make the update visible!"

# test installation of created distribution
.PHONY: testdist
testdist: $(TARBALL)
	cp $(TARBALL) $(TMP)
	rm -rf $(TMPDIR)
	cd $(TMP) && tar xzfv $(TARBALL)
	cd $(TMPDIR) && $(MAKE) install
	cd $(TMPDIR) && $(MAKE) runtest
	rm -rf $(TMPDIR)
	rm -rf $(TMP)/$(TARBALL)
	@echo "Integration test successfully completed."

# Directories containing development stuff only
DEV_DIRS = benchmarks debug docs experiments talks

# Clean all files that should not be included in a distribution
.PHONY: cleandist
cleandist:
ifneq ($(CURDIR), $(TMPDIR))
	$(error cleandist target called outside $(TMPDIR))
endif
	rm -rf .dist-modules .git .gitignore .gitmodules
	cd currytools              && rm -rf .git .gitignore
	cd frontend/curry-base     && rm -rf .git .gitignore dist
	cd frontend/curry-frontend && rm -rf .git .gitignore dist
	rm -rf lib-trunk
	cd utils                   && $(MAKE) cleanall
	rm -rf $(BINDIR)
	rm -rf $(DEV_DIRS)
	rm -rf $(LOCALPKG)

$(TARBALL): $(COMP) $(CYMAKE) $(MANUAL)
	rm -rf $(TMPDIR)
	# clone current git repository
	git clone . $(TMPDIR)
	# adopt paths for submodules
	cat .dist-modules | sed 's|ROOT|$(ROOT)|' > $(TMPDIR)/.gitmodules
	# check out submodules
	cd $(TMPDIR) && git submodule init && git submodule update
	# create local binary directory
	mkdir -p $(TMPDIR)/bin/.local
	# copy frontend binary
	cp -p $(CYMAKE) $(TMPDIR)/bin/
	# copy bootstrap compiler
	cp -p $(COMP) $(TMPDIR)/bin/.local/
	# generate compiler and REPL in order to have the bootstrapped
	# Haskell translations in the distribution
	cd $(TMPDIR) && $(MAKE) Compile       # translate compiler
	cd $(TMPDIR) && $(MAKE) REPL          # translate REPL
	cd $(TMPDIR) && $(MAKE) clean         # clean object files
	cd $(TMPDIR) && $(MAKE) typeinference # precompile typeinference
	cd $(TMPDIR) && $(MAKE) cleandist     # delete unnessary files
	# copy documentation
	mkdir -p $(TMPDIR)/docs
	cp $(MANUAL) $(TMPDIR)/docs
	# update Makefile
	cat Makefile \
	  | sed -e "/^# SNIP FOR DISTRIBUTION/,\$$d" \
	  | sed 's|^GLOBALINSTALL *=.*$$|GLOBALINSTALL   = yes|' \
	  | sed 's|^PROFILING *=.*$$|PROFILING   = no|' \
	  | sed 's|^COMPILERDATE *:=.*$$|COMPILERDATE    = $(COMPILERDATE)|' \
	  > $(TMPDIR)/Makefile
	# Zip it!
	cd $(TMP) && tar cf $(FULLNAME).tar $(FULLNAME) && gzip $(FULLNAME).tar
	mv $(TMP)/$(TARBALL) ./$(TARBALL)
	chmod 644 ./$(TARBALL)
	rm -rf $(TMPDIR)
	@echo "----------------------------------"
	@echo "Distribution $(TARBALL) generated."

##############################################################################
# Development targets
##############################################################################

# bootstrap the compiler
.PHONY: bootstrap
bootstrap: $(COMP)

.PHONY: frontend
frontend: $(CYMAKE)

.PHONY: Compile
Compile: $(PKGDB) $(INSTALLCURRY) scripts copylibs
	cd src && $(MAKE) CompileBoot

.PHONY: REPL
REPL: $(PKGDB) $(INSTALLCURRY) scripts copylibs
	cd src && $(MAKE) REPLBoot

.PHONY: typeinference
typeinference:
	cd currytools && $(MAKE) typeinference

# install the benchmark system
.PHONY: benchmarks
benchmarks:
	cd benchmarks && $(MAKE)

$(COMP): | $(INSTALLCURRY) $(PKGDB) $(CYMAKE) $(CLEANCURRY) scripts copylibs
	cd src && $(MAKE) bootstrap

# Peform a full bootstrap - distribution - installation - uninstallation
# lifecycle to test consistency of the whole process.
.PHONY: roundtrip
roundtrip:
	$(MAKE) maintainer-clean
	$(MAKE) bootstrap
	$(MAKE) kernel
	$(MAKE) dist
	$(MAKE) testdist
	mv $(TARBALL) $(FULLNAME)-$(shell date +%Y%m%d).tar.gz

# This is a debugging target showing you the current setting of variables.
.PHONY: config
config:
	@$(foreach V, \
          $(sort $(.VARIABLES)), \
	  $(if $(filter-out environment% default automatic, \
          $(origin $V)),$(info $V = $($V))))
	@true
