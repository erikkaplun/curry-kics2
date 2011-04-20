# generate various tools for handling Curry programs:

.PHONY: all
all:    ImportCalls

# generate saved state for interface generator:
ImportCalls: ImportCalls.curry
	../bin/kics2 :l ImportCalls :save :q

.PHONY: clean
clean: 
	../bin/cleancurry
