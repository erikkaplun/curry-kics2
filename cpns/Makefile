# Translate CPNS demon for fast startup:

# Required:
# - installed curry2prolog system

CPNSD=CPNSD
LOGFILE="/tmp/PAKCS_CPNSD_LOG"

${CPNSD}: ../lib/CPNS.curry
	cd ../lib && ../bin/kics2 :set v2 :load CPNS :save :q
	mv ../lib/CPNS ${CPNSD}

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
	../bin/cleancurry
	rm -f ${CPNSD}
