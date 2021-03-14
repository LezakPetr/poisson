
ALL: examples poisson.pdf

.PHONY: examples equations

examples:
	cd examples && $(MAKE)

equations:
	(cd equations && swi-prolog.swipl --stack-limit=100M preprocess.pl)
	(cd out/ && swi-prolog.swipl --stack-limit=100M ch_logika.pl)
	(cd out/ && swi-prolog.swipl --stack-limit=100M ch_cisla.pl)

poisson.pdf: poisson.tex equations ch_*.tex ap_*.tex
	pdflatex $<
	pdflatex $<

