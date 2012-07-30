##############################################################################
# Installation of KICS2 shell scripts
##############################################################################

UNAME := $(shell uname)

ifeq ($(UNAME), Linux)
BINS = $(patsubst %.sh, $(BINDIR)/%, $(wildcard *.sh))
BATS =
else
BINS = $(patsubst %.sh, $(BINDIR)/%, makecurrycgi.sh kics2.sh)
BATS = $(patsubst %, $(BINDIR)/%, $(wildcard *.bat))
endif

.PHONY: all
all: $(BINS) $(BATS)
	@echo "Scripts generated in directory $(<D)."

.PHONY: clean
clean:
	rm -f $(BINS)

$(BINDIR)/%: %.sh
	mkdir -p $(@D)
	sed "s|^KICS2HOME=.*$$|KICS2HOME=$(ROOT)|" < $< > $@
	chmod 755 $@

$(BINDIR)/%.bat: %.bat
	mkdir -p $(@D)
	sed "s|^set KICS2HOME=.*$$|set KICS2HOME=$(ROOT)|" < $< > $@
	chmod 755 $@
