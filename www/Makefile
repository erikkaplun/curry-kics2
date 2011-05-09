# Translate programs for managing cgi server processes:

.PHONY: all
all: submitform Registry

submitform: ../lib/HtmlCgi.curry ../lib/NamedSocket.curry ../lib/CPNS.curry
	cd ../lib && ../bin/kics2 :l HtmlCgi :save :q
	mv ../lib/HtmlCgi submitform

Registry: Registry.curry ../lib/HtmlCgi.curry
	../bin/kics2 :l Registry :save :q

.PHONY: cleanServers
cleanServers:
	./Registry clean

.PHONY: show
show:
	./Registry show

.PHONY: showload
showload:
	./Registry showload

.PHONY: sketch
sketch:
	./Registry sketch

.PHONY: showall
showall:
	./Registry showall

.PHONY: stop
stop:
	./Registry stop

.PHONY: kill
kill:
	./Registry kill

.PHONY: clean
clean: 
	rm -f submitform
	../bin/cleancurry


