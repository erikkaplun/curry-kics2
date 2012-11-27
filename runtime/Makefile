# Generate the kics2-runtime.cabal and install the package

# Tools
CABAL   = cabal
GHC_PKG = ghc-pkg

# Name of the package
PACKAGE     = kics2-runtime
# Name of the cabal file
CABAL_FILE  = $(PACKAGE).cabal
# idsupply directory to use
IDSUPPLYDIR = idsupplyioref

.PHONY: install
install: $(CABAL_FILE)
	$(CABAL) install -O2

.PHONY: unregister
unregister:
	-$(GHC_PKG) unregister $(PACKAGE)-$(VERSION)

.PHONY: clean
clean:
	-$(CABAL) clean
	rm -f $(CABAL_FILE)

$(CABAL_FILE): ../Makefile Makefile
ifndef VERSION
	$(error VERSION is not defined. Please use 'make' on top-level)
endif
	echo "Name:           $(PACKAGE)"                        >  $@
	echo "Version:        $(VERSION)"                        >> $@
	echo "Description:    The runtime environment for KiCS2" >> $@
	echo "License:        OtherLicense"                      >> $@
	echo "Author:         Fabian Reck"                       >> $@
	echo "Maintainer:     fre@informatik.uni-kiel.de"        >> $@
	echo "Build-Type:     Simple"                            >> $@
	echo "Cabal-Version:  >= 1.9.2"                          >> $@
	echo ""                                                  >> $@
	echo "Library"                                           >> $@
	echo "  Build-Depends:"                                  >> $@
	echo "      base, containers, ghc, mtl"                  >> $@
	echo "    , parallel-tree-search, tree-monad"            >> $@
	echo "  Exposed-modules:"                                >> $@
	echo "      Basics, CurryException, Debug, IDSupply"     >> $@
	echo "    , Installation, MonadList, MonadSearch"        >> $@
	echo "    , PrimTypes, SafeExec"                         >> $@
	echo "  Other-modules:"                                  >> $@
	echo "      ConstStore, ID, Search, Solver"              >> $@
	echo "    , Strategies, Types"                           >> $@
	echo "  hs-source-dirs: ., $(IDSUPPLYDIR)"               >> $@
