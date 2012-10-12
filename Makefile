########################################################################
# Makefile for KiCS2 compiler suite
########################################################################

# Is this a global installation (with restricted functionality)(yes/no)?
GLOBALINSTALL=yes
# The major version number:
MAJORVERSION    = 0
# The minor version number:
MINORVERSION    = 2
# The revision version number:
REVISIONVERSION = 2
# Complete version:
export VERSION := $(MAJORVERSION).$(MINORVERSION).$(REVISIONVERSION)
# The version date
COMPILERDATE = 08/10/12
# The installation date
INSTALLDATE := $(shell date)

# the root directory
export ROOT     = $(CURDIR)
# binary directory and executables
export BINDIR   = $(ROOT)/bin
# Directory where the libraries are located:
export LIBDIR   = $(ROOT)/lib
# Directory where local executables are stored:
export LOCALBIN = $(BINDIR)/.local
# The compiler binary
export COMP     = $(LOCALBIN)/kics2c
# The REPL binary
export REPL     = $(LOCALBIN)/kics2i
# The default options for the REPL
export REPL_OPTS = :set v2 :set -ghci
# The frontend binary
export CYMAKE   = $(LOCALBIN)/cymake

# The Haskell installation info
export INSTALLHS     = $(ROOT)/runtime/Installation.hs
# The Curry installation info
export INSTALLCURRY  = $(ROOT)/src/Installation.curry
# The version information for the manual:
MANUALVERSION = $(ROOT)/docs/src/version.tex
# Logfiles for make:
MAKELOG = make.log
BOOTLOG = boot.log

# The path to the Glasgow Haskell Compiler:
export GHC     := $(shell which ghc)
export GHC-PKG := $(dirname $(GHC))ghc-pkg
# The path to the package configuration file
PKGCONF := $(shell $(GHC-PKG) --user -v0 list | head -1 | sed "s/:$$//" | sed "s/\\\\/\//g" )

# main (default) target: starts installation with logging
.PHONY: all
all:
	${MAKE} installwithlogging

# bootstrap the compiler
.PHONY: bootstrap
bootstrap: ${INSTALLCURRY} frontend scripts
	@rm -f ${BOOTLOG}
	@echo "Bootstrapping started at `date`" > ${BOOTLOG}
	cd src && ${MAKE} bootstrap 2>&1 | tee -a ../${BOOTLOG}
	@echo "Bootstrapping finished at `date`" >> ${BOOTLOG}
	@echo "Bootstrap process logged in file ${BOOTLOG}"

# install the complete system and log the installation process
.PHONY: installwithlogging
installwithlogging:
	@rm -f ${MAKELOG}
	@echo "Make started at `date`" > ${MAKELOG}
	${MAKE} install 2>&1 | tee -a ${MAKELOG}
	@echo "Make finished at `date`" >> ${MAKELOG}
	@echo "Make process logged in file ${MAKELOG}"

# install the complete system if the kics2 compiler is present
.PHONY: install
install: kernel
	cd cpns  && $(MAKE) # Curry Port Name Server demon
	cd tools && $(MAKE) # various tools
	cd www   && $(MAKE) # scripts for dynamic web pages
	$(MAKE) manual
	# make everything accessible:
	chmod -R go+rX .

# uninstall globally installed cabal packages
.PHONY: uninstall
uninstall:
ifeq ($(GLOBALINSTALL),yes)
	cd frontend && $(MAKE) unregister
	cd lib      && $(MAKE) unregister
	cd runtime  && $(MAKE) unregister
	@echo "All globally installed cabal packages have been unregistered."
endif
	rm -rf $(HOME)/.kics2rc $(HOME)/.kics2rc.bak $(HOME)/.kics2i_history
	@echo "Just remove this directory to finish uninstallation."

# install a kernel system without all tools
.PHONY: kernel
kernel: $(INSTALLCURRY) frontend scripts
	cd src && $(MAKE)
ifeq ($(GLOBALINSTALL),yes)
	cd lib     && $(MAKE) unregister
	cd runtime && $(MAKE) unregister
	cd runtime && $(MAKE)
	# compile all libraries for a global installation
	cd lib     && $(MAKE) compilelibs
	cd lib     && $(MAKE) installlibs
	cd lib     && $(MAKE) acy CYMAKE=$(CYMAKE)
endif

.PHONY: scripts
scripts: utils/cleancurry
	cd scripts && $(MAKE) ROOT=$(shell utils/pwd)
	cp $< $(BINDIR)

.PHONY: frontend
frontend: utils/cabaldir
	cd frontend && $(MAKE)

# install required cabal packages
.PHONY: installhaskell
installhaskell:
	cabal update
	cabal install network
	cabal install parallel
	cabal install tree-monad
	cabal install parallel-tree-search
	cabal install mtl

.PHONY: clean
clean:
	rm -f *.log
	rm -f ${INSTALLHS} ${INSTALLCURRY}
	cd benchmarks && ${MAKE} clean
	cd cpns       && ${MAKE} clean
	@if [ -d lib/.curry/kics2 ] ; then \
	  cd lib/.curry/kics2 && rm -f *.hi *.o ; \
	fi
	@if [ -d lib/meta/.curry/kics2 ] ; then \
	  cd lib/meta/.curry/kics2 && rm -f *.hi *.o ; \
	fi
	cd runtime    && ${MAKE} clean
	cd src        && ${MAKE} clean
	cd tools      && ${MAKE} clean
	cd www        && ${MAKE} clean

# clean everything (including compiler binaries)
.PHONY: cleanall
cleanall: clean
	cd src && $(MAKE) cleanall
	$(BINDIR)/cleancurry -r
	rm -rf ${LOCALBIN}
#	cd scripts && $(MAKE) clean

##############################################################################
# Building the compiler itself
##############################################################################

# generate module with basic installation information:
${INSTALLCURRY}: ${INSTALLHS}
	cp $< $@

${INSTALLHS}: Makefile utils/pwd utils/which
	@if [ ! -x "${GHC}" ] ; then \
	  echo "No executable 'ghc' found in path!" && exit 1; \
	fi
	echo "-- This file is automatically generated, do not change it!" > $@
	echo "module Installation where" >> $@
	echo "" >> $@
	echo 'compilerName :: String' >> $@
	echo 'compilerName = "KiCS2 Curry -> Haskell Compiler"' >> $@
	echo "" >> $@
	echo 'installDir :: String' >> $@
	echo 'installDir = "$(shell utils/pwd)"' >> $@
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
	echo 'ghcExec :: String' >> $@
	echo 'ghcExec = "\"$(shell utils/which ghc)\" -no-user-package-conf -package-conf \"${PKGCONF}\""' >> $@
	echo "" >> $@
	echo 'installGlobal :: Bool' >> $@
ifeq ($(GLOBALINSTALL),yes)
	echo 'installGlobal = True' >> $@
else
	echo 'installGlobal = False' >> $@
endif

utils/%:
	cd utils && $(MAKE) $(@F)

##############################################################################
# Create documentation for system libraries:
##############################################################################

.PHONY: libdoc
libdoc:
	@if [ ! -r $(BINDIR)/currydoc ] ; then \
	  echo "Cannot create library documentation: currydoc not available!" ; exit 1 ; fi
	@rm -f ${MAKELOG}
	@echo "Make libdoc started at `date`" > ${MAKELOG}
	@cd lib && ${MAKE} doc 2>&1 | tee -a ../${MAKELOG}
	@echo "Make libdoc finished at `date`" >> ${MAKELOG}
	@echo "Make libdoc process logged in file ${MAKELOG}"

##############################################################################
# Create the KiCS2 manual
##############################################################################

.PHONY: manual
manual:
	# generate manual, if necessary:
	@if [ -d docs/src ] ; then \
	  ${MAKE} ${MANUALVERSION} && cd docs/src && ${MAKE} install ; \
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
# Create distribution versions of the complete system as tar file kics2.tar.gz
##############################################################################

# temporary directory to create distribution version
TMP     =/tmp
FULLNAME=kics2-$(VERSION)
TMPDIR  =$(TMP)/$(FULLNAME)
TARBALL =$(FULLNAME).tar.gz

# generate a source distribution of KICS2:
.PHONY: dist
dist: $(COMP)
	# remove old distribution
	rm -rf $(TARBALL) ${TMPDIR}
	# initialise git repository
	git clone . ${TMPDIR}
	cd ${TMPDIR} && git submodule init && git submodule update
	# create local binary directory
	mkdir -p ${TMPDIR}/bin/.local
	# copy frontend binary into distribution
	if [ -x $(CYMAKE) ] ; then \
	  cp -pr $(CYMAKE) $(TMPDIR)/bin/.local/ ; \
	else \
	  cd $(TMPDIR) && $(MAKE) frontend ; \
	fi
	# copy bootstrap compiler
	cp $(COMP) ${TMPDIR}/bin/.local/
	# generate compile and REPL in order to have the bootstrapped
	# Haskell translations in the distribution
	cd ${TMPDIR} && ${MAKE} Compile   # translate compiler
	cd ${TMPDIR} && ${MAKE} REPL      # translate REPL
	cd ${TMPDIR} && ${MAKE} clean     # clean object files
	cd ${TMPDIR} && ${MAKE} cleandist # delete unnessary files
	# copy documentation
	@if [ -f docs/Manual.pdf ] ; then \
	  mkdir -p ${TMPDIR}/docs ; \
	  cp docs/Manual.pdf ${TMPDIR}/docs ; \
	fi
	# update Makefile
	cat Makefile | sed -e "/^# SNIP FOR DISTRIBUTION/,\$$d"       \
	             | sed 's|^GLOBALINSTALL=.*$$|GLOBALINSTALL=yes|' \
	             > ${TMPDIR}/Makefile
	# Zip it!
	cd $(TMP) && tar cf $(FULLNAME).tar $(FULLNAME) && gzip $(FULLNAME).tar
	mv $(TMP)/$(TARBALL) ./$(TARBALL)
	chmod 644 ./$(TARBALL)
	rm -rf ${TMPDIR}
	@echo "----------------------------------"
	@echo "Distribution $(TARBALL) generated."

# publish the distribution files in the local web pages
HTMLDIR=${HOME}/public_html/kics2/download
.PHONY: publish
publish:
	cp $(TARBALL) docs/INSTALL.html ${HTMLDIR}
	chmod -R go+rX ${HTMLDIR}
	@echo "Don't forget to run 'update-kics2' to make the update visible!"

# Directories containing development stuff only
DEV_DIRS=benchmarks debug docs experiments papers talks

# Clean all files that should not be included in a distribution
.PHONY: cleandist
cleandist:
	rm -rf .git .gitmodules .gitignore
	rm -rf lib/.git
	cd frontend/curry-base     && rm -rf .git .gitignore dist
	cd frontend/curry-frontend && rm -rf .git .gitignore dist
	cd utils && make cleanall
	rm -rf $(BINDIR) # clean executables
	rm -rf $(DEV_DIRS)

##############################################################################
# Development only targets
##############################################################################

.PHONY: Compile
Compile: ${INSTALLCURRY} scripts
	cd src && ${MAKE} CompileBoot

.PHONY: REPL
REPL: ${INSTALLCURRY} scripts
	cd src && ${MAKE} REPLBoot

.PHONY: testdist
testdist: $(TARBALL)
	

# Peform a full bootstrap - distribution - installation
# lifecycle to test consistency of the whole process
# WARNING: This installation will corrupt any existing global KICS2
# installation for the current user which shares the exact same version!
# This is because the runtime and libraries cabal packages would be
# reinstalled and, later on, unregistered.
.PHONY: test
test:
	# clean up
	$(MAKE) cleanall
	rm -rf $(BINDIR)
	# bootstrap!
	$(MAKE) bootstrap
	# make distribution
	make dist
	# test installation
	cp $(TARBALL) $(TMP)
	rm -rf $(TMPDIR)
	cd $(TMP) && tar xzfv $(TARBALL)
	cd $(TMPDIR) && $(MAKE)
	cd $(TMPDIR) && $(MAKE) uninstall
	rm -rf $(TMPDIR)
	rm -rf $(TMP)/$(TARBALL)
	@echo "Integration test successfully completed."
