## Makefile for running iscam
## Targets: 
##		all:   -clean the directory, produce .tex and .pdf files


all: ICCAT_BBL ICCAT_TEX
	pdflatex ./ICCAT.tex
	pdflatex ICCAT.tex

ICCAT_AUX: ICCAT_TEX
	pdflatex ICCAT.tex

ICCAT_BBL: ICCAT_AUX
	bibtex ICCAT

ICCAT_TEX: ICCAT.Rnw
	Rscript Run.R ICCAT

clean: 
	rm *.bbl
	rm *.blg
	rm *.aux
	rm *.log
	rm ICCAT.pdf

