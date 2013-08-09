# Translate CPNS demon to support named ports in distributed Curry programs:

CPNSD=CPNSD
LOGFILE="/tmp/CurryPNSD.log"

${CPNSD}: ../lib/CPNS.curry
	$(KICS2) :set v2 :load CPNSD :save :q

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
