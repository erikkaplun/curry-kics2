# Translate programs for managing cgi server processes:

.PHONY: all
all: submitform Registry

submitform: SubmitForm.curry ../lib/HtmlCgi.curry ../lib/NamedSocket.curry ../lib/CPNS.curry
	../bin/kics2 :set v2 :l SubmitForm :save :q
	mv SubmitForm submitform

Registry: Registry.curry ../lib/HtmlCgi.curry
	../bin/kics2 :set v2 :l Registry :save :q

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


