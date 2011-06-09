# Makefile to generate CurryBrowser tool

LIB=../../lib
META=${LIB}/meta
TOOLS=..

# generate saved state for currytest program:
BrowserGUI: BrowserGUI.curry ShowFlatCurry.curry Imports.curry \
	    AnalysisTypes.curry BrowserAnalysis.curry ShowGraph.curry \
	    FlatCurryGoodies.curry BrowserPropertyFile.curry \
	    ${TOOLS}/AddTypes.curry ${TOOLS}/ImportCalls.curry \
	    ${LIB}/GUI.curry ${LIB}/IOExts.curry  ${LIB}/System.curry \
	    ${META}/FlatCurry.curry ${META}/FlatCurryShow.curry \
	    analysis/*.curry
	echo "patchReadmeVersion" | \
	 ../../bin/kics2 :set v2 :set path analysis:${TOOLS} :l BrowserGUI :save
	(cd ../../bin ; rm -f currybrowse ; \
	                ln -s ../tools/browser/BrowserGUI currybrowse)

.PHONY: clean
clean: 
	../../bin/cleancurry -r
	rm -f ../../bin/currybrowse
