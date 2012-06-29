# Makefile to generate the kics2-runtime.cabal
# and install the runtime environment as ghc package

# The name of the ghc package
PKGNAME=kics2-runtime
# The name of the cabal file
CABAL=${PKGNAME}.cabal
# The name of the idsupplydirectory to use
IDSUPPDIR=idsupplyioref

.PHONY: installpkg
installpkg: ${CABAL}
	cabal install -O2 # --force-reinstalls

${CABAL}: ../Makefile Makefile
 ifdef MAJORVERSION
  ifdef MINORVERSION
    ifdef REVISIONVERSION
	echo "Name:           ${PKGNAME}" > ${CABAL}
	echo "Version:        ${MAJORVERSION}.${MINORVERSION}.${REVISIONVERSION}" >> ${CABAL}
	echo "Description:    The runtime environment for KiCS2" >> ${CABAL}
	echo "License:        OtherLicense" >> ${CABAL}
	echo "Author:         Fabian Reck" >> ${CABAL}
	echo "Maintainer:     fre@informatik.uni-kiel.de" >> ${CABAL}
	echo "Build-Type:     Simple" >> ${CABAL}
	echo "Cabal-Version:  >= 1.9.2" >> ${CABAL}
	echo "" >> ${CABAL}
	echo "Library" >> ${CABAL}
	echo "  Build-Depends: base, containers, ghc, mtl, parallel-tree-search" >> ${CABAL}
	echo "                 , tree-monad" >> ${CABAL}
	echo "  Exposed-modules: Basics, Debug, CurryException, PrimTypes, IDSupply," >> ${CABAL}
	echo "                   SafeExec, MonadList, Installation, MonadSearch" >> ${CABAL}
	echo "  Other-modules:   ConstStore, ID, Search, Solver," >> ${CABAL}
	echo "                   Strategies, Types" >> ${CABAL}
	echo "  hs-source-dirs: ., ${IDSUPPDIR}" >> ${CABAL}
    else
	@echo "Error: REVISIONVERSION is not defined"
	@echo "This target should only be called from the toplevel makefile."
	exit 1
    endif
  else
	@echo "Error: MINORVERSION is not defined"
	@echo "This target should only be called from the toplevel makefile."
	exit 1
  endif
 else
	@echo "Error: MAJORVERSION is not defined"
	@echo "This target should only be called from the toplevel makefile."
	exit 1
 endif

.PHONY: clean
clean:
	rm -f ${CABAL}
	rm -rf dist
	rm -f *.hi *.o *.hi-boot *.o-boot
	rm -f ./idsupply*/*.hi ./idsupply*/*.o
