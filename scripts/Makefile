##############################################################################
# Installation of KICS2 shell scripts
##############################################################################

SRCS = $(wildcard *.sh)
BINS = $(patsubst %.sh, $(BINDIR)/%, $(SRCS))

.PHONY: all
all: $(BINS)
	@echo "Scripts generated."

.PHONY: clean
clean:
	rm -f $(BINS)

$(BINDIR)/%: %.sh
	mkdir -p $(BINDIR)
	sed "s|^KICS2HOME=.*$$|KICS2HOME=$(ROOT)|" < $< > $@
	chmod 755 $@
