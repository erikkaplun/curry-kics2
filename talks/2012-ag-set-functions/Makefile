pdfs = talk.pdf

all: $(pdfs)

clean:
	rm -rf *.aux *.dvi *.log *.nav *.out *.ptb *.snm *.toc *.vrb *~ *.backup # latex

cleanall: clean
	rm -rf $(pdfs)

talk.pdf: talk.tex
	pdflatex $<
	pdflatex $<
