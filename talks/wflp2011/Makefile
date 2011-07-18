current: talk.pdf

clean:
	rm -rf *.aux *.log *.nav *.out *.ptb *.snm *.toc *.vrb *~ *.backup

%.pdf: %.tex
	pdflatex $<
	pdflatex $<
