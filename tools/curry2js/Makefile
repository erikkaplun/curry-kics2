# Generate executable for the Curry2JavaScript translator:

LIB=../../lib

.PHONY: all
all:    Curry2JS

# generate saved state for the Curry2JavaScript translator:
Curry2JS: Curry2JS.curry ${LIB}/JavaScript.curry \
	        ${LIB}/meta/CompactFlatCurry.curry ${LIB}/meta/FlatCurry.curry
	../../bin/kics2 :l Curry2JS :save :q

clean: 
	../../bin/cleancurry
