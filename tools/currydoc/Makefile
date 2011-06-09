# Makefile for generating CurryDoc tool

LIB=../../lib
META=${LIB}/meta
ANADIR=../analysis

# generate executable for currydoc program:
CurryDoc: CurryDoc.curry CurryDocRead.curry CurryDocHtml.curry \
	        CurryDocTeX.curry ${META}/FlexRigid.curry \
	        ${LIB}/CategorizedHtmlList.curry \
	        ${LIB}/Distribution.curry ${LIB}/PropertyFile.curry \
	        ${META}/FlatCurry.curry ${LIB}/HTML.curry \
	        ${ANADIR}/AnaOverlapping.curry ${ANADIR}/AnaIndeterminism.curry \
	        ${ANADIR}/AnaCompleteness.curry ${ANADIR}/AnaOpComplete.curry \
	        ${ANADIR}/AnaDependency.curry 
	../../bin/kics2 :set path ${ANADIR} :l CurryDoc :save :q
	(cd ../../bin ; rm -f currydoc ; \
	                ln -s ../tools/currydoc/CurryDoc currydoc)


.PHONY: clean
clean: 
	../../bin/cleancurry
	rm -f ../../bin/currydoc
