########################################################################
# Makefile for Benchmarks program
########################################################################

.PHONY: all
all: Benchmarks
	cd benchmark_papers && $(MAKE)

# generate binary for Benchmarks program
Benchmarks: Benchmarks.curry
	$(CLEANCURRY)
	$(REPL) $(REPL_OPTS) :load Benchmarks :save :q

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
	cd benchmark_papers && $(MAKE) clean
