########################################################################
# Makefile for KiCS2 compiler suite
########################################################################

# Some information about this installation
# ----------------------------------------

# Is this a global installation (with restricted flexibility)(yes/no)?
GLOBALINSTALL   = yes
# The major version number
MAJORVERSION    = 0
# The minor version number
MINORVERSION    = 3
# The revision version number
REVISIONVERSION = 0
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
# The frontend binary
export CYMAKE       = $(BINDIR)/cymake$(EXE_SUFFIX)
# The cleancurry binary
export CLEANCURRY   = $(BINDIR)/cleancurry$(EXE_SUFFIX)
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

# install the complete system if the kics2 compiler is present
.PHONY: install
install: kernel alltools
	cd cpns       && $(MAKE) # Curry Port Name Server demon
	cd www        && $(MAKE) # scripts for dynamic web pages
	$(MAKE) manual
	# make everything accessible:
	chmod -R go+rX .

# install the benchmark system
.PHONY: benchmarks
benchmarks:
	cd benchmarks && $(MAKE)

# uninstall globally installed cabal packages
.PHONY: uninstall
uninstall:
	rm -rf $(HOME)/.kics2rc $(HOME)/.kics2rc.bak $(HOME)/.kics2i_history
	@echo "Just remove this directory to finish uninstallation."

# install the kernel system (binaries and libraries)
.PHONY: kernel
kernel: $(PWD) $(WHICH) $(PKGDB) frontend scripts
	$(MAKE) $(INSTALLCURRY) INSTALLPREFIX="$(shell $(PWD))" GHC="$(shell $(WHICH) "$(GHC)")"
	cd src && $(MAKE)
ifeq ($(GLOBALINSTALL),yes)
	cd lib     && $(MAKE) unregister
	cd runtime && $(MAKE) unregister
	cd runtime && $(MAKE)
	# compile all libraries for a global installation
	cd lib     && $(MAKE) compilelibs
	cd lib     && $(MAKE) installlibs
	cd lib     && $(MAKE) acy
endif

.PHONY: alltools
alltools:
	cd currytools && $(MAKE) # various tools
	cd tools      && $(MAKE) # various tools

# create package database
$(PKGDB):
	"$(GHC-PKG)" init $@
	$(CABAL) update
	$(CABAL_INSTALL) $(filter-out $(GHC_LIBS),$(ALLDEPS))

.PHONY: frontend
frontend:
	cd frontend && $(MAKE)

.PHONY: scripts
scripts: $(PWD) $(CLEANCURRY)
	cd scripts && $(MAKE) ROOT=$(shell $(PWD))

$(CLEANCURRY): utils/cleancurry$(EXE_SUFFIX)
	mkdir -p $(@D)
	cp $< $@

utils/%:
	cd utils && $(MAKE) $(@F)

.PHONY: clean
clean: $(CLEANCURRY)
	rm -f *.log
	rm -f $(INSTALLHS) $(INSTALLCURRY)
	cd cpns       && $(MAKE) clean
	@if [ -d lib/.curry/kics2 ] ; then \
	  cd lib/.curry/kics2 && rm -f *.hi *.o ; \
	fi
	@if [ -d lib/meta/.curry/kics2 ] ; then \
	  cd lib/meta/.curry/kics2 && rm -f *.hi *.o ; \
	fi
	cd runtime    && $(MAKE) clean
	cd src        && $(MAKE) clean
	cd currytools && $(MAKE) clean
	cd frontend   && $(MAKE) clean
	cd tools      && $(MAKE) clean
	cd utils      && $(MAKE) clean
	cd www        && $(MAKE) clean
	@if [ -d benchmarks ] ; then \
	  cd benchmarks && $(MAKE) clean ; \
	fi

# clean everything (including compiler binaries)
.PHONY: cleanall
cleanall: clean
	cd src   && $(MAKE) cleanall
	cd utils && $(MAKE) cleanall
	$(CLEANCURRY) -r
	rm -rf ${LOCALBIN} $(CYMAKE) $(LOCALPKG)
	cd scripts && $(MAKE) clean
	rm $(CLEANCURRY)

.PHONY: maintainer-clean
maintainer-clean: cleanall
	rm -rf $(BINDIR)

##############################################################################
# Building the compiler itself
##############################################################################

# generate module with basic installation information
$(INSTALLCURRY): $(INSTALLHS)
	cp $< $@

$(INSTALLHS): Makefile
ifneq ($(shell test -x "$(GHC)" ; echo $$?), 0)
	$(error "No executable 'ghc' found. You may use 'make <target> GHC=<path>')
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
	echo 'ghcOptions = "$(subst ",\",$(GHC_OPTS)) -package kics2-runtime -package kics2-libraries"' >> $@
	echo "" >> $@
	echo 'installGlobal :: Bool' >> $@
ifeq ($(GLOBALINSTALL),yes)
	echo 'installGlobal = True' >> $@
else
	echo 'installGlobal = False' >> $@
endif

##############################################################################
# Create documentation for system libraries:
##############################################################################

.PHONY: libdoc
libdoc:
	@if [ ! -r $(BINDIR)/currydoc ] ; then \
	  echo "Cannot create library documentation: currydoc not available!" ; exit 1 ; fi
	@rm -f ${MAKELOG}
	@echo "Make libdoc started at `date`" > ${MAKELOG}
	@cd lib && $(MAKE) doc 2>&1 | tee -a ../${MAKELOG}
	@echo "Make libdoc finished at `date`" >> ${MAKELOG}
	@echo "Make libdoc process logged in file ${MAKELOG}"

##############################################################################
# Create the KiCS2 manual
##############################################################################

.PHONY: manual
manual:
	# generate manual, if necessary:
	@if [ -d docs/src ] ; then \
	  $(MAKE) ${MANUALVERSION} && cd docs/src && $(MAKE) install ; \
	fi

${MANUALVERSION}: Makefile
	echo '\\newcommand{\\kicsversiondate}'         >  $@
	echo '{Version $(VERSION) of ${COMPILERDATE}}' >> $@

.PHONY: cleanmanual
cleanmanual:
	if [ -d docs/src ] ; then \
	  cd docs/src && $(MAKE) clean ; \
	fi

# SNIP FOR DISTRIBUTION - DO NOT REMOVE THIS COMMENT

##############################################################################
# Distribution targets
##############################################################################

# temporary directory to create distribution version
TMP     =/tmp
FULLNAME=kics2-$(VERSION)
TMPDIR  =$(TMP)/$(FULLNAME)
TARBALL =$(FULLNAME).tar.gz

# generate a source distribution of KICS2:
.PHONY: dist
dist:
	# remove old distribution
	rm -f $(TARBALL)
	$(MAKE) $(TARBALL)

# publish the distribution files in the local web pages
HTMLDIR=${HOME}/public_html/kics2/download
.PHONY: publish
publish: $(TARBALL)
	cp $(TARBALL) docs/INSTALL.html ${HTMLDIR}
	chmod -R go+rX ${HTMLDIR}
	@echo "Don't forget to run 'update-kics2' to make the update visible!"

# test distribution installation
.PHONY: testdist
testdist: $(TARBALL)
	cp $(TARBALL) $(TMP)
	rm -rf $(TMPDIR)
	cd $(TMP) && tar xzfv $(TARBALL)
	cd $(TMPDIR) && $(MAKE)
	cd $(TMPDIR) && $(MAKE) uninstall
	rm -rf $(TMPDIR)
	rm -rf $(TMP)/$(TARBALL)
	@echo "Integration test successfully completed."

# Directories containing development stuff only
DEV_DIRS=benchmarks debug docs experiments talks

# Clean all files that should not be included in a distribution
.PHONY: cleandist
cleandist:
ifneq ($(CURDIR), $(TMPDIR))
	$(error cleandist target called outside $(TMPDIR))
endif
	rm -rf .dist-modules .git .gitmodules .gitignore
	cd lib        && rm -rf .git .gitignore
	cd currytools && rm -rf .git .gitignore
	cd frontend/curry-base     && rm -rf .git .gitignore dist
	cd frontend/curry-frontend && rm -rf .git .gitignore dist
	rm -rf $(BINDIR)
	rm -rf $(LOCALPKG)
	cd utils && $(MAKE) cleanall
	rm -rf $(DEV_DIRS)

$(TARBALL): $(COMP)
	rm -rf $(TMPDIR)
	# initialise git repository
	git clone . $(TMPDIR)
	cat .dist-modules | sed 's|ROOT|$(ROOT)|' > $(TMPDIR)/.gitmodules
	cd $(TMPDIR) && git submodule init && git submodule update
	# create local binary directory
	mkdir -p $(TMPDIR)/bin/.local
	# copy frontend binary into distribution
	if [ -x $(CYMAKE) ] ; then \
	  cp -pr $(CYMAKE) $(TMPDIR)/bin/ ; \
	else \
	  cd $(TMPDIR) && $(MAKE) frontend ; \
	fi
	# copy bootstrap compiler
	cp $(COMP) $(TMPDIR)/bin/.local/
	# generate compile and REPL in order to have the bootstrapped
	# Haskell translations in the distribution
	cd $(TMPDIR) && $(MAKE) Compile       # translate compiler
	cd $(TMPDIR) && $(MAKE) REPL          # translate REPL
	cd $(TMPDIR) && $(MAKE) clean         # clean object files
	cd $(TMPDIR) && $(MAKE) typeinference # precompile typeinference
	cd $(TMPDIR) && $(MAKE) cleandist     # delete unnessary files
	# copy documentation
	@if [ -f docs/Manual.pdf ] ; then \
	  mkdir -p $(TMPDIR)/docs ; \
	  cp docs/Manual.pdf $(TMPDIR)/docs ; \
	fi
	# update Makefile
	cat Makefile | sed -e "/^# SNIP FOR DISTRIBUTION/,\$$d"         \
	             | sed 's|^GLOBALINSTALL *=.*$$|GLOBALINSTALL=yes|' \
	             | sed 's|^COMPILERDATE *:=.*$$|COMPILERDATE =$(COMPILERDATE)|' \
	             > $(TMPDIR)/Makefile
	# Zip it!
	cd $(TMP) && tar cf $(FULLNAME).tar $(FULLNAME) && gzip $(FULLNAME).tar
	mv $(TMP)/$(TARBALL) ./$(TARBALL)
	chmod 644 ./$(TARBALL)
	rm -rf $(TMPDIR)
	@echo "----------------------------------"
	@echo "Distribution $(TARBALL) generated."

$(COMP):
	$(MAKE) bootstrap

##############################################################################
# Development targets
##############################################################################

BOOTLOG = boot.log

# bootstrap the compiler with logging
.PHONY: bootstrapwithlogging
bootstrapwithlogging:
	@rm -f ${BOOTLOG}
	@echo "Bootstrapping started at `date`" > ${BOOTLOG}
	$(MAKE) bootstrap 2>&1 | tee -a ../${BOOTLOG}
	@echo "Bootstrapping finished at `date`" >> ${BOOTLOG}
	@echo "Bootstrap process logged in file ${BOOTLOG}"

# bootstrap the compiler
.PHONY: bootstrap
bootstrap: $(PKGDB) $(INSTALLCURRY) frontend scripts
	cd src && $(MAKE) bootstrap

.PHONY: Compile
Compile: $(PKGDB) $(INSTALLCURRY) scripts
	cd src && $(MAKE) CompileBoot

.PHONY: REPL
REPL: $(PKGDB) $(INSTALLCURRY) scripts
	cd src && $(MAKE) REPLBoot

.PHONY: typeinference
typeinference:
	cd currytools && $(MAKE) typeinference

# Peform a full bootstrap - distribution - installation - uninstallation
# lifecycle to test consistency of the whole process.
# WARNING: This installation will corrupt any existing global KICS2
# installation for the current user which shares the exact same version!
# This is because the runtime and libraries cabal packages would be
# reinstalled and, later on, unregistered.
.PHONY: roundtrip
roundtrip:
	$(MAKE) maintainer-clean
	$(MAKE) bootstrap
	$(MAKE) dist
	$(MAKE) testdist
	mv $(TARBALL) $(FULLNAME)-$(shell date +%Y%m%d).tar.gz

.PHONY: config
config:
	@$(foreach V, \
          $(sort $(.VARIABLES)), \
	  $(if $(filter-out environment% default automatic, \
          $(origin $V)),$(info $V = $($V))))
	@true
