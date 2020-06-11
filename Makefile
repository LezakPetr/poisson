
ALL: examples poisson.pdf

.PHONY: examples

examples:
	cd examples && $(MAKE)

poisson.pdf: poisson.tex
	pdflatex $<
	pdflatex $<

