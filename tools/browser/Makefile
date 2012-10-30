# Makefile to generate CurryBrowser tool

LIB=../../lib
META=${LIB}/meta
TOOLS=..
CURRYTOOLS=../../currytools

# generate saved state for currytest program:
BrowserGUI: BrowserGUI.curry ShowFlatCurry.curry Imports.curry \
	    AnalysisTypes.curry BrowserAnalysis.curry ShowGraph.curry \
	    FlatCurryGoodies.curry BrowserPropertyFile.curry \
	    ${TOOLS}/AddTypes.curry \
	    ${CURRYTOOLS}/importcalls/ImportCalls.curry \
	    ${LIB}/GUI.curry ${LIB}/IOExts.curry  ${LIB}/System.curry \
	    ${META}/FlatCurry.curry ${META}/FlatCurryShow.curry \
	    analysis/*.curry
	../../bin/kics2 ${REPL_OPTS} :set path analysis:$(TOOLS):$(CURRYTOOLS)/importcalls \
	                :l BrowserGUI :save :eval "patchReadmeVersion" :q
	(cd ../../bin ; rm -f currybrowse ; \
	                ln -s ../tools/browser/BrowserGUI currybrowse)

.PHONY: clean
clean: 
	../../bin/cleancurry -r
	rm -f ../../bin/currybrowse
