DOT=$(wildcard output/*.dot)
PDF=$(DOT:.dot=.pdf)
KASIM=KaSim -i ../story.ka -i ../egfr.ka -l 4 -trace t.json

all: ccores

clean:
	rm -rf output

clean_all: clean
	rm -f *~ *# *.xml *.dat

output:
	mkdir output

output/t.json: output
	cd output; KaSim -i ../story.ka -i ../egfr.ka -l 4 -trace t.json

ccores: output/t.json
	cd output; ../../../KaFlow t.json -o "story." -r "6/R-G_1"
	@make pdf


pdf: $(PDF)

%.pdf: %.dot
	dot -Tpdf $< -o $@
