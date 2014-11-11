# Makefile to generate "Executable Benchmark Papers" tool:

# tool binary:
TOOLBIN=execpaper

.PHONY: install
install: $(TOOLBIN)

.PHONY: uninstall
uninstall:
	rm -f $(TOOLBIN)

$(TOOLBIN): ExecuteBenchmarkPaper.curry
	$(REPL) $(REPL_OPTS) :load ExecuteBenchmarkPaper \
	                     :save "main \"$(CURDIR)\"" :quit
	mv ExecuteBenchmarkPaper $(TOOLBIN)

.PHONY: clean
clean: uninstall
	$(CLEANCURRY) -r

