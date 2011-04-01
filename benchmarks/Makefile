########################################################################
# Makefile for Benchmarks program
########################################################################

.PHONY: all
all: Benchmarks.state

# generate saved state for Curry->FLVM compiler:
Benchmarks.state: Benchmarks.curry
	cleancurry
	pakcs -s Benchmarks

# publish all relevant benchmarks in the web:
.PHONY: publish
publish:
	cd suite && tar cvzf ../benchmarks.tgz *.curry *.hs *.pl
	chmod 644 benchmarks.tgz
	mv benchmarks.tgz ${HOME}/public_html/repos/kics2

.PHONY: clean
clean:
	cleancurry
