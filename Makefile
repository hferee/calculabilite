PDFLATEX=lualatex
LATEXMK?=latexmk -pdf -pdflatex=$(PDFLATEX) --file-line-error --shell-escape --synctex=1
TARGETS=$(subst .tex,.pdf,$(wildcard *.tex))

.PHONY: all clean

all: $(TARGETS)

%.pdf:	%.tex
	$(LATEXMK) $^



clean:
	rm -rdf *.dot *.pdf *.out *.aux *.gvpr *.graphviz *.toc *.fls *.dvi *.log *.fdb_latexmk *.synctex.gz
