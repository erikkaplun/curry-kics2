# Translate CPNS demon to support named ports in distributed Curry programs:

CPNSD=CPNSD
LOGFILE="/tmp/CurryPNSD.log"

${CPNSD}: ../lib/CPNS.curry
	$(REPL) $(REPL_OPTS) :load CPNSD :save :q

# Shows the currently registered ports:
.PHONY: show
show: ${CPNSD}
	./${CPNSD} show

# Shows the log file of the demon:
.PHONY: showlog
showlog:
	cat ${LOGFILE}

.PHONY: clean
clean:
	./stop
	$(CLEANCURRY)
	rm -f ${CPNSD}
