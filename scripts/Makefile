##############################################################################
# Installation of KICS2 shell scripts
##############################################################################

ifdef WINDOWS
BINS =
BATS = $(patsubst %, $(BINDIR)/%, $(wildcard *.bat))
else
# Unixes
BINS = $(patsubst %.sh, $(BINDIR)/%, $(wildcard *.sh))
BATS =
endif

.PHONY: all
all: $(BINS) $(BATS)
	@echo "Scripts generated in directory $(<D)."

.PHONY: cleanall
cleanall:
	rm -f $(BINS) $(BATS)

$(BINDIR)/%: %.sh
	mkdir -p $(@D)
	sed "s|^KICS2HOME=.*$$|KICS2HOME=$(ROOT)|" < $< > $@
	chmod 755 $@

$(BINDIR)/%.bat: %.bat
	mkdir -p $(@D)
	sed "s|^set KICS2HOME=.*$$|set KICS2HOME=$(ROOT)|" < $< > $@
	chmod 755 $@
