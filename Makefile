
ALL: examples poisson.pdf

.PHONY: examples

examples:
	cd examples && $(MAKE)

poisson.pdf: poisson.tex ch_*.tex ap_*.tex
	pdflatex $<
	pdflatex $<

