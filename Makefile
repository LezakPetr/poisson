
ALL: examples poisson.pdf

.PHONY: examples equations

examples:
	cd examples && $(MAKE)

equations:
	cd equations && $(MAKE)

poisson.pdf: poisson.tex equations ch_*.tex ap_*.tex
	pdflatex $<
	pdflatex $<

