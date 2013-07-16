# To format the KiCS2 User Manual and other documents

TEXFILES = currybrowser.tex currydoc.tex currytest.tex erd2curry.tex \
           extensions.tex syntax.tex external_ops.tex libraries.tex  \
           markdown_syntax.tex overview.tex preface.tex problems.tex \
           usage.tex version.tex

HTMLFILES = ../markdown_syntax.html ../INSTALL.html \
            ../GITINSTALL.html ../RELNOTES.html

MANUAL = ../Manual.pdf

KICS2 = ../../bin/kics2

.PHONY: pdf
pdf : manual.pdf

manual.pdf: manual.tex $(TEXFILES) ../../lib/TEXDOC/LAST
	pdflatex manual
	#bibtex manual
	pdflatex manual
	makeindex manual.idx
	pdflatex manual

$(MANUAL): manual.pdf
	thumbpdf manual
	pdflatex manual
	rm -f $@
	cp $< $@

syntax.tex: syntax.sty

# format markdown syntax description as LaTeX
markdown_syntax.tex: markdown_syntax.txt
	$(KICS2) :add Markdown :eval 'readFile "$<" >>= writeFile "$@" . markdownText2LaTeX' :q

# format markdown syntax description as HTML page
../markdown_syntax.html: markdown_syntax.txt
	$(KICS2) :add Markdown :eval 'readFile "$<" >>= writeFile "$@" . markdownText2CompleteHTML "Markdown Syntax"' :q

# format installation description as HTML page
../INSTALL.html: ../../INSTALL.txt
	$(KICS2) :add Markdown :eval 'readFile "$<" >>= writeFile "$@" . markdownText2CompleteHTML "KiCS2 Installation"' :q

# format repository installation description as HTML page
../GITINSTALL.html: ../../GITINSTALL.txt
	$(KICS2) :add Markdown :eval 'readFile "$<" >>= writeFile "$@" . markdownText2CompleteHTML "KiCS2 Repository"' :q

# format release notes as HTML page
../RELNOTES.html: ../../RELNOTES.txt
	$(KICS2) :add Markdown :eval 'readFile "$<" >>= writeFile "$@" . markdownText2CompleteHTML "KiCS2 Release Notes"' :q

../../lib/TEXDOC/LAST:
	$(MAKE) libdoc

# generate documentation for all libraries
.PHONY: libdoc
libdoc:
	@cd ../../lib && $(MAKE) texdoc
	@rm -f lib && ln -s ../../lib/TEXDOC lib

.PHONY: install
install: $(HTMLFILES) libdoc $(MANUAL)

.PHONY: clean
clean:
	rm -f markdown_syntax.tex
	rm -f *.aux *.idx *.ilg *.ind *.log *.out *.toc *.tpt

.PHONY: cleanall
cleanall: clean
	rm -f manual.pdf $(MANUAL) $(HTMLFILES)
