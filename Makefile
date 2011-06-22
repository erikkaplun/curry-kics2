########################################################################
# Makefile for ID compiler
########################################################################

# The major version number:
MAJORVERSION=0
# The minor version number:
MINORVERSION=1
# The version date:
COMPILERDATE=16/06/11
# The Haskell installation info
INSTALLHS=runtime/Installation.hs
# The Curry installation info
INSTALLCURRY=src/Installation.curry
# Logfile for make:
MAKELOG=make.log
BOOTLOG=boot.log

.PHONY: all
all:
	${MAKE} installwithlogging

# bootstrap the compiler using PAKCS
.PHONY: bootstrap
bootstrap: ${INSTALLCURRY}
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
install: ${INSTALLCURRY} REPL Compile
	cd cpns  && ${MAKE} # Curry Port Name Server demon
	cd tools && ${MAKE} # various tools
	cd www   && ${MAKE} # scripts for dynamic web pages
	chmod -R go+rX .

.PHONY: Compile
Compile: ${INSTALLCURRY}
	cd src ; ${MAKE} CompileBoot

.PHONY: REPL
REPL: ${INSTALLCURRY}
	cd src ; ${MAKE} REPLBoot

# generate module with basic installation information:
${INSTALLCURRY}: ${INSTALLHS}
	cp ${INSTALLHS} ${INSTALLCURRY}

${INSTALLHS}: Makefile
	echo "-- This file is automatically generated, do not change it!" > ${INSTALLHS}
	echo "module Installation where" >> ${INSTALLHS}
	echo "" >> ${INSTALLHS}
	echo 'compilerName :: String' >> ${INSTALLHS}
	echo 'compilerName = "KiCS2 Curry -> Haskell Compiler"' >> ${INSTALLHS}
	echo "" >> ${INSTALLHS}
	echo 'installDir :: String' >> ${INSTALLHS}
	echo 'installDir = "'`pwd`'"' >> ${INSTALLHS}
	echo "" >> ${INSTALLHS}
	echo 'majorVersion :: Int' >> ${INSTALLHS}
	echo 'majorVersion = ${MAJORVERSION}' >> ${INSTALLHS}
	echo "" >> ${INSTALLHS}
	echo 'minorVersion :: Int' >> ${INSTALLHS}
	echo 'minorVersion = ${MINORVERSION}' >> ${INSTALLHS}
	echo "" >> ${INSTALLHS}
	echo 'compilerDate :: String' >> ${INSTALLHS}
	echo 'compilerDate = "'${COMPILERDATE}'"' >> ${INSTALLHS}
	echo "" >> ${INSTALLHS}
	echo 'installDate :: String' >> ${INSTALLHS}
	echo 'installDate = "'`date`'"' >> ${INSTALLHS}

# install required cabal packages

.PHONY: installhaskell
installhaskell:
	cabal install parallel
	cabal install tree-monad
	cabal install parallel-tree-search
	cabal install mtl

.PHONY: clean
clean:
	rm -f *.log
	rm -f ${INSTALLHS} ${INSTALLCURRY}
	cd src   ; ${MAKE} clean
	cd lib/.curry/kics2 && rm -f *.hi *.o
	cd cpns  ; ${MAKE} clean
	cd tools ; ${MAKE} clean
	cd www   ; ${MAKE} clean

# clean everything (including compiler binaries)
.PHONY: cleanall
cleanall: clean
	bin/cleancurry -r
	rm -f bin/idc bin/idci


################################################################################
# Create distribution versions of the complete system as tar files kics2.tar.gz:

# temporary directory to create distribution version
KICS2DIST=/tmp/kics2
# directory with distribution of mcc front-end:
MCCPARSERHOME=/home/mh/lehrstuhl/frontend/mcc
MCCPARSERDIST=${MCCPARSERHOME}/dist
MCCSRCDIST=${MCCPARSERDIST}/mcc_for_pakcs_src.tar.gz

# install mcc parser sources (without make) from current distribution:
.PHONY: installmcc
installmcc:
	rm -rf mccparser
	gunzip -c ${MCCSRCDIST} | tar xf -

# generate a source distribution of KICS2:
.PHONY: dist
dist:
	cd ${MCCPARSERHOME} && ${MAKE} dist  # make mcc frontend distribution
	rm -rf kics2.tar.gz ${KICS2DIST}     # remove old distribution
	git clone . ${KICS2DIST}             # create copy of git version
	cp Makefile ${KICS2DIST}/Makefile  # temporary
	cd ${KICS2DIST} && ${MAKE} cleandist # delete unnessary files
	cd ${KICS2DIST} && ${MAKE} installmcc # install front-end sources
	cd bin && cp idc idci ${KICS2DIST}/bin # copy bootstrap compiler
	cd ${KICS2DIST} && ${MAKE} Compile   # translate compiler
	cd ${KICS2DIST} && ${MAKE} REPL      # translate REPL
	cd ${KICS2DIST} && ${MAKE} clean     # clean object files
	cd ${KICS2DIST}/bin && rm idc idci   # clean executables
	sed -e "/distribution/,\$$d" < Makefile > ${KICS2DIST}/Makefile
	cd /tmp && tar cf kics2.tar kics2 && gzip kics2.tar
	mv /tmp/kics2.tar.gz .
	chmod 644 kics2.tar.gz
	rm -rf ${KICS2DIST}
	@echo "----------------------------------------------------------------"
	@echo "Distribution kics2.tar.gz generated."

#
# Clean all files that should not be included in a distribution
#
.PHONY: cleandist
cleandist:
	rm -rf .git .gitignore bin/.gitignore
	rm -rf benchmarks papers talks tests
	rm -f TODO compilerdoc.wiki
