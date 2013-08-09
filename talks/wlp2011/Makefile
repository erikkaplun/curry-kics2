pdfs = slides.pdf

all: $(pdfs)

clean:
	rm -rf *.aux *.dvi *.log *.nav *.out *.ptb *.snm *.toc *.vrb *~ *.backup # latex

cleanall: clean
	rm -rf $(pdfs)

slides.pdf: $(wildcard *.tex *.jpg)
	pdflatex $<
	pdflatex $<
