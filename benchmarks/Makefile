########################################################################
# Makefile for Benchmarks program
########################################################################

.PHONY: all
all: Benchmarks

# generate binary for Benchmarks program
Benchmarks: Benchmarks.curry
	$(CLEANCURRY)
	$(KICS2) :l Benchmarks :save :q

# publish all relevant benchmarks in the web
.PHONY: publish
publish:
	cd suite && tar cvzf ../benchmarks.tgz README *.curry *.hs *.pl
	chmod 644 benchmarks.tgz
	mv benchmarks.tgz $(HOME)/public_html/repos/kics2

.PHONY: clean
clean:
	$(CLEANCURRY)
	rm -f bench.log
