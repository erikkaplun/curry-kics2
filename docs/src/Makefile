# To format the KiCS2 User Manual

.PHONY: pdf
pdf : manual.pdf

manual.pdf: manual.tex
	pdflatex manual
	bibtex manual
	pdflatex manual
	makeindex manual.idx
	thumbpdf manual
	pdflatex manual
	rm -f thumb* manual.out

../Manual.pdf: manual.pdf
	rm -f ../Manual.pdf
	cp manual.pdf ../Manual.pdf

# generate documentation for all libraries
.PHONY: libdoc
libdoc:
	@cd ../../lib && ${MAKE} texdoc
	@rm -f lib && ln -s ../../lib/TEXDOC lib

.PHONY: install
install:
	${MAKE} libdoc
	${MAKE} ../Manual.pdf

.PHONY: clean
clean:
	/bin/mv manual.tex manual.texx
	/bin/mv manual.bib manual.bibx
	/bin/rm -f manual.?? manual.??? version-defs.sty
	/bin/mv manual.texx manual.tex
	/bin/mv manual.bibx manual.bib
	/bin/rm -f ../Manual.pdf


