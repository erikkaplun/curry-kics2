styles := $(wildcard *.sty)
pdfs   := talk.pdf handout.pdf handoutA4.pdf

all: $(pdfs)

clean:
	rm -rf *.aux *.dvi *.log *.nav *.out *.ptb *.snm *.toc *.vrb *~ *.backup # latex

cleanall: clean
	rm -rf $(pdfs)

talk.pdf: talk.tex $(styles)
	pdflatex $<
	pdflatex $<

handout.pdf: handout.tex talk.tex $(styles)
	pdflatex $<
	pdflatex $<

handoutA4.pdf: handout.pdf
	pdftops -paperw 842 -paperh 595 -expand $< - | ps2pdf - $@

