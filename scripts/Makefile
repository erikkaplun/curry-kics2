##############################################################################
# Installation of KICS2 shell scripts
##############################################################################

BINS = $(patsubst %.sh, $(BINDIR)/%, $(wildcard *.sh))

.PHONY: all
all: $(BINS)
	@echo "Scripts generated in directory $(<D)."

.PHONY: clean
clean:
	rm -f $(BINS)

$(BINDIR)/%: %.sh
	mkdir -p $(@D)
	sed "s|^KICS2HOME=.*$$|KICS2HOME=$(ROOT)|" < $< > $@
	chmod 755 $@
