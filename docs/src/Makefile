# To format the KiCS2 User Manual and other documents

HTMLFILES = $(DOCDIR)/markdown_syntax.html $(DOCDIR)/INSTALL.html \
            $(DOCDIR)/GITINSTALL.html $(DOCDIR)/RELNOTES.html

MANUAL = $(DOCDIR)/Manual.pdf

.PHONY: pdf
pdf : manual.pdf

.PHONY: install
install: $(HTMLFILES) $(MANUAL)

.PHONY: clean
clean:
	rm -f markdown_syntax.tex
	rm -f *.aux *.idx *.ilg *.ind *.log *.out *.toc *.tpt

.PHONY: cleanall
cleanall: clean
	rm -f manual.pdf $(MANUAL) $(HTMLFILES)
	rm -f lib version.tex

$(MANUAL): manual.pdf
	thumbpdf manual
	pdflatex manual
	rm -f $@
	cp $< $@

manual.pdf: $(wildcard *.tex *.bbl *.jpg) version.tex markdown_syntax.tex libdoc
	pdflatex manual
	#bibtex manual
	pdflatex manual
	makeindex manual.idx
	pdflatex manual

# generate documentation for all libraries
.PHONY: libdoc
libdoc:
	cd $(LIBDIR) && $(MAKE) texdoc

syntax.tex: syntax.sty

# format markdown syntax description as LaTeX
markdown_syntax.tex: markdown_syntax.txt
	$(REPL) :add Markdown :eval 'readFile "$<" >>= writeFile "$@" . markdownText2LaTeX' :q

# format markdown syntax description as HTML page
$(DOCDIR)/markdown_syntax.html: markdown_syntax.txt
	$(REPL) :add Markdown :eval 'readFile "$<" >>= writeFile "$@" . markdownText2CompleteHTML "Markdown Syntax"' :q

# format installation description as HTML page
$(DOCDIR)/INSTALL.html: $(ROOT)/INSTALL.txt
	$(REPL) :add Markdown :eval 'readFile "$<" >>= writeFile "$@" . markdownText2CompleteHTML "KiCS2 Installation"' :q

# format repository installation description as HTML page
$(DOCDIR)/GITINSTALL.html: $(ROOT)/GITINSTALL.txt
	$(REPL) :add Markdown :eval 'readFile "$<" >>= writeFile "$@" . markdownText2CompleteHTML "KiCS2 Repository"' :q

# format release notes as HTML page
$(DOCDIR)/RELNOTES.html: $(ROOT)/RELNOTES.txt
	$(REPL) :add Markdown :eval 'readFile "$<" >>= writeFile "$@" . markdownText2CompleteHTML "KiCS2 Release Notes"' :q
