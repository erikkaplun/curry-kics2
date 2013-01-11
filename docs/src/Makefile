# To format the KiCS2 User Manual and other documents

KICS2=../../bin/kics2

.PHONY: pdf
pdf : manual.pdf

manual.pdf: manual.tex version.tex currydoc.tex markdown_syntax.tex \
	    ../../lib/TEXDOC/LAST
	pdflatex manual
	#bibtex manual
	pdflatex manual
	makeindex manual.idx
	pdflatex manual

../Manual.pdf: manual.pdf
	thumbpdf manual
	pdflatex manual
	rm -f ../Manual.pdf
	cp manual.pdf ../Manual.pdf

# format markdown syntax description as LaTeX
markdown_syntax.tex: markdown_syntax.txt
	$(KICS2) :add Markdown :eval 'readFile "markdown_syntax.txt" >>= writeFile "markdown_syntax.tex" . markdownText2LaTeX' :q

# format markdown syntax description as HTML page
../markdown_syntax.html: markdown_syntax.txt
	$(KICS2) :add Markdown :eval 'readFile "markdown_syntax.txt" >>= writeFile "../markdown_syntax.html" . markdownText2CompleteHTML "Markdown Syntax"' :q

# format installation description as HTML page
../INSTALL.html: ../../INSTALL.txt
	$(KICS2) :add Markdown :eval 'readFile "../../INSTALL.txt" >>= writeFile "../INSTALL.html" . markdownText2CompleteHTML "KiCS2 Installation"' :q

# format repository installation description as HTML page
../GITINSTALL.html: ../../GITINSTALL.txt
	$(KICS2) :add Markdown :eval 'readFile "../../GITINSTALL.txt" >>= writeFile "../GITINSTALL.html" . markdownText2CompleteHTML "KiCS2 Repository"' :q

# format release notes as HTML page
../RELNOTES.html: ../../RELNOTES.txt
	$(KICS2) :add Markdown :eval 'readFile "../../RELNOTES.txt" >>= writeFile "../RELNOTES.html" . markdownText2CompleteHTML "KiCS2 Release Notes"' :q

../../lib/TEXDOC/LAST:
	$(MAKE) libdoc

# generate documentation for all libraries
.PHONY: libdoc
libdoc:
	@cd ../../lib && $(MAKE) texdoc
	@rm -f lib && ln -s ../../lib/TEXDOC lib

.PHONY: install
install:
	$(MAKE) ../markdown_syntax.html
	$(MAKE) ../INSTALL.html
	$(MAKE) ../GITINSTALL.html
	$(MAKE) ../RELNOTES.html
	$(MAKE) libdoc
	$(MAKE) ../Manual.pdf

.PHONY: clean
clean:
	/bin/rm -f markdown_syntax.tex ../markdown_syntax.html
	/bin/mv manual.tex manual.texx
	/bin/mv manual.bbl manual.bblx
	/bin/rm -f manual.?? manual.???
	/bin/mv manual.texx manual.tex
	/bin/mv manual.bblx manual.bbl
	/bin/rm -f ../Manual.pdf thumb* version.tex
