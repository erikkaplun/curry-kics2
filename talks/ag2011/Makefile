pdfs := $(patsubst %.tex,%.pdf,$(wildcard *.tex))

all: $(pdfs)

clean:
	rm -rf *.aux *.log *.nav *.out *.ptb *.snm *.toc *.vrb *~ # latex

cleanall: clean
	rm -rf *.pdf

%.pdf: %.tex
	pdflatex $<
	pdflatex $<
