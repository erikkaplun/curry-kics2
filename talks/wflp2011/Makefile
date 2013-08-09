pdf = talk.pdf

.PHONY: current clean cleanall
current: $(pdf)

clean:
	rm -rf *.aux *.log *.nav *.out *.ptb *.snm *.toc *.vrb *~ *.backup

cleanall: clean
	rm -f $(pdf)

%.pdf: %.tex
	pdflatex $<
	pdflatex $<
