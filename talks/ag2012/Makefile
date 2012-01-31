styles := $(wildcard *.sty)
pdfs := $(patsubst %.tex,%.pdf,$(wildcard *.tex))

all: $(pdfs)

clean:
	rm -rf *.aux *.dvi *.log *.nav *.out *.ptb *.snm *.toc *.vrb *~ # latex

cleanall: clean
	rm -rf *.pdf

%_handout.pdf: %_handout.tex %.tex $(styles)
	pdflatex $<
	pdflatex $<

%.pdf: %.tex $(styles)
	pdflatex $<
	pdflatex $<
