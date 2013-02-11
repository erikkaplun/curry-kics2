##############################################################################
# Installation of KICS2 shell scripts
##############################################################################

UNAME := $(shell uname)

ifeq ($(UNAME), Linux)
# Linux
BINS = $(patsubst %.sh, $(BINDIR)/%, $(wildcard *.sh))
BATS =
else ifeq ($(UNAME), Darwin)
# Mac OS
BINS = $(patsubst %.sh, $(BINDIR)/%, $(wildcard *.sh))
BATS =
else ifeq ($(UNAME), SunOS)
# Sun OS
BINS = $(patsubst %.sh, $(BINDIR)/%, $(wildcard *.sh))
BATS =
else ifeq ($(findstring MINGW, $(UNAME)), MINGW)
# Windows
BINS =
BATS = $(patsubst %, $(BINDIR)/%, $(wildcard *.bat))
else
$(error "Unknown system '$(UNAME)')
endif

.PHONY: all
all: $(BINS) $(BATS)
	@echo "Scripts generated in directory $(<D)."

.PHONY: clean
clean:
	rm -f $(BINS) $(BATS)

$(BINDIR)/%: %.sh
	mkdir -p $(@D)
	sed "s|^KICS2HOME=.*$$|KICS2HOME=$(ROOT)|" < $< > $@
	chmod 755 $@

$(BINDIR)/%.bat: %.bat
	mkdir -p $(@D)
	sed "s|^set KICS2HOME=.*$$|set KICS2HOME=$(ROOT)|" < $< > $@
	chmod 755 $@
