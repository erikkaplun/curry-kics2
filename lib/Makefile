# Makefile for various compilations of the system libraries,
# in particular, to generate the documentation

CYMAKE=`pwd`/../bin/cymake
CYMAKEPARAMS=--no-verb --no-warn --no-overlap-warn 

# directory for HTML documentation files:
DOCDIR=CDOC
# directory for LaTeX documentation files:
TEXDOCDIR=TEXDOC
# the currydoc program:
CURRYDOC=`pwd`/../bin/currydoc

LIB_CURRY = Prelude.curry \
	    AllSolutions.curry Array.curry Assertion.curry \
	    CategorizedHtmlList.curry \
            Char.curry Combinatorial.curry \
	    Constraint.curry CPNS.curry CSV.curry  \
            Dequeue.curry Directory.curry Distribution.curry  \
            FileGoodies.curry FiniteMap.curry Float.curry \
	    Global.curry GraphInductive.curry GUI.curry \
	    HTML.curry HtmlCgi.curry HtmlParser.curry \
	    Integer.curry IO.curry IOExts.curry \
	    JavaScript.curry \
            KeyDatabaseSQLite.curry \
	    List.curry Mail.curry Maybe.curry \
	    NamedSocket.curry \
	    Parser.curry Pretty.curry \
	    Profile.curry PropertyFile.curry \
            Random.curry Read.curry ReadNumeric.curry ReadShowTerm.curry \
            RedBlackTree.curry SearchTree.curry SetRBT.curry \
	    Socket.curry Sort.curry System.curry \
            TableRBT.curry Time.curry Traversal.curry \
            Unsafe.curry URL.curry WUI.curry WUIjs.curry \
	    XML.curry XmlConv.curry \
	    meta/AbstractCurry.curry meta/AbstractCurryPrinter.curry \
	    meta/CompactFlatCurry.curry \
	    meta/CurryStringClassifier.curry \
            meta/FlatCurry.curry \
	    meta/FlatCurryRead.curry meta/FlatCurryShow.curry \
	    meta/FlatCurryGoodies.curry \
	    meta/FlatCurryXML.curry \
	    meta/FlexRigid.curry \
	    meta/PrettyAbstract.curry 

LIB_FCY   = `echo $(LIB_CURRY:%.curry=.curry/%.fcy) | sed 's|\.curry/meta/|meta/.curry/|g'`
LIB_ACY   = `echo $(LIB_CURRY:%.curry=.curry/%.acy) | sed 's|\.curry/meta/|meta/.curry/|g'`
LIB_HTML  = $(LIB_CURRY:.curry=.html)
LIB_TEX   = $(LIB_CURRY:.curry=.tex)
LIB_NAMES = `echo $(LIB_CURRY) | sed 's|meta/||g'` # lib names without meta/ prefix

.PHONY: all
all: fcy acy

.PHONY: fcy
fcy:
	${MAKE} $(LIB_FCY)

.PHONY: acy
acy:
	${MAKE} $(LIB_ACY)

# generate all FlatCurry files in subdirectory .curry:
.curry/%.fcy: %.curry
	"${CYMAKE}" --flat ${CYMAKEPARAMS} $*

meta/.curry/%.fcy: meta/%.curry
	"${CYMAKE}" --flat ${CYMAKEPARAMS} $*

# generate all AbstractCurry files in subdirectory .curry:
.curry/%.acy: %.curry
	"${CYMAKE}" --acy ${CYMAKEPARAMS} $*

meta/.curry/%.acy: meta/%.curry
	"${CYMAKE}" --acy ${CYMAKEPARAMS} $*

# create HTML documentation files for system libraries
.PHONY: doc
doc: $(LIB_CURRY)
	@mkdir -p "${DOCDIR}"
	@cd "${DOCDIR}" && rm -f meta DOINDEX && ln -s . meta
	@cd "${DOCDIR}" && ${MAKE} -f ../Makefile $(LIB_HTML)
	@if [ -f "${DOCDIR}/DOINDEX" ] ; then ${MAKE} htmlindex ; fi
	@cd "${DOCDIR}" && rm -f meta DOINDEX

.PHONY: htmlindex
htmlindex:
	@echo "Generating index pages for Curry libraries:"
	@echo $(LIB_NAMES)
	@"${CURRYDOC}" --onlyindexhtml "${DOCDIR}" $(LIB_NAMES)

# generate individual documentations for libraries:
%.html: ../%.curry
	@touch DOINDEX
	cd .. && "${CURRYDOC}" --noindexhtml "${DOCDIR}" $*

meta/%.html: ../meta/%.curry
	@touch DOINDEX
	cd .. && "${CURRYDOC}" --noindexhtml "${DOCDIR}" $*

# create LaTeX documentation files for system libraries
.PHONY: texdoc
texdoc: $(LIB_CURRY)
	@mkdir -p "${TEXDOCDIR}"
	@if [ ! -f "${TEXDOCDIR}/LAST" ] ; then touch "${TEXDOCDIR}/LAST" ; fi
	@cd "${TEXDOCDIR}" && rm -f meta && ln -s . meta
	@cd "${TEXDOCDIR}" && ${MAKE} -f ../Makefile $(LIB_TEX)
	@cd "${TEXDOCDIR}" && rm -f meta

# generate individual LaTeX documentations for libraries:
%.tex: ../%.curry
	cd .. && "${CURRYDOC}" --tex "${TEXDOCDIR}" $*
	touch LAST

meta/%.tex: ../meta/%.curry
	cd .. && "${CURRYDOC}" --tex "${TEXDOCDIR}" $*
	touch LAST


# clean all generated files
.PHONY: clean
clean:
	rm -f "${DOCDIR}"/*
	../bin/cleancurry
	cd meta && ../../bin/cleancurry
