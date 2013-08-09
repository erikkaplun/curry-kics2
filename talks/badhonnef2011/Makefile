pdfs = talk.pdf
pics = $(patsubst %.odg,%.pdf,$(wildcard *.odg))

all: $(pdfs)

clean:
	rm -rf *.aux *.dvi *.log *.nav *.out *.ptb *.snm *.toc *.vrb *~ *.backup # latex

cleanall: clean
	rm -rf $(pdfs)

talk.pdf: talk.tex $(pics)
	pdflatex $<
	pdflatex $<
