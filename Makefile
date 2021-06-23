
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

out/ap_derivace_zakladnich_funkci.tex: ap_derivace_zakladnich_funkci.tex
	(cd equations && swi-prolog.swipl --stack-limit=100M preprocess_derivace_zakladnich_funkci.pl)
	(cd out/ && swi-prolog.swipl --stack-limit=100M ap_derivace_zakladnich_funkci.pl)

out/ap_integraly_zakladnich_funkci.tex: ap_integraly_zakladnich_funkci.tex
	(cd equations && swi-prolog.swipl --stack-limit=100M preprocess_integraly_zakladnich_funkci.pl)
	(cd out/ && swi-prolog.swipl --stack-limit=100M ap_integraly_zakladnich_funkci.pl)

out/ch_diferencialni_pocet.tex: ch_diferencialni_pocet.tex
	(cd equations && swi-prolog.swipl --stack-limit=100M preprocess_diferencialni_pocet.pl)
	(cd out/ && swi-prolog.swipl --stack-limit=100M ch_diferencialni_pocet.pl)

equations: out/ch_logika.tex out/ch_cisla.tex out/ch_komplexni_cisla.tex out/ap_derivace_zakladnich_funkci.tex out/ap_integraly_zakladnich_funkci.tex out/ch_diferencialni_pocet.tex

poisson.pdf: poisson.tex equations ch_*.tex ap_*.tex
	pdflatex $<
	pdflatex $<

clean:
	rm out/* *.aux

