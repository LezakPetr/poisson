
ALL: examples poisson.pdf

.PHONY: examples equations preprocess clean

examples:
	cd examples && $(MAKE)

out/ch_logika.tex: ch_logika.tex
	(cd equations && swi-prolog.swipl --stack-limit=100M preprocess_logika.pl)
	(cd out/ && swi-prolog.swipl --stack-limit=100M ch_logika.pl)

out/ch_cisla.tex: ch_cisla.tex
	(cd equations && swi-prolog.swipl --stack-limit=100M preprocess_cisla.pl)
	(cd out/ && swi-prolog.swipl --stack-limit=100M ch_cisla.pl)

out/ch_komplexni_cisla.tex: ch_komplexni_cisla.tex
	(cd equations && swi-prolog.swipl --stack-limit=100M preprocess_komplexni_cisla.pl)
	(cd out/ && swi-prolog.swipl --stack-limit=100M ch_komplexni_cisla.pl)

equations: out/ch_logika.tex out/ch_cisla.tex out/ch_komplexni_cisla.tex

poisson.pdf: poisson.tex equations ch_*.tex ap_*.tex
	pdflatex $<
	pdflatex $<

clean:
	rm out/* *.aux

