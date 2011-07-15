both: talk1.pdf talk2.pdf

clean:
	rm -rf *.aux *.log *.nav *.out *.ptb *.snm *.toc *.vrb *~ # latex

%.pdf: %.tex
	pdflatex $<
	pdflatex $<
