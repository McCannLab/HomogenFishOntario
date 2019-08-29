ms = inst/ms/ms_homogen_VF.md
si = inst/ms/ms_si_VF.md
msold = inst/ms/msV3/ms_homogen_V3.md
msp := $(wildcard $(ms) $(msold))
mst := $(patsubst %.md,%.tex, $(msp))
diff = inst/ms/diff.tex
rsp = inst/ms/responses_V3.md
ref = /home/kevcaz/Dropbox/kevcaz.bib
csl = /home/kevcaz/Zotero/styles/global-change-biology.csl
pflags = -N --bibliography=$(ref) --pdf-engine=pdflatex --csl=$(csl)

$(patsubst _main/%, %, $(mainr))


all: NAMESPACE

NAMESPACE: $(rfun)
	Rscript --no-site-file --no-init-file $(rscr) 1

cleaning:
	rm inst/assets/


msmain: $(ms)
	pandoc $< -o $(patsubst %.md, %.pdf, $<) $(pflags)
	pandoc $< -o $(patsubst %.md, %.docx, $<) $(pflags)

mssi: $(si)
	pandoc $< -o $(patsubst %.md, %.pdf, $<) $(pflags)

mstex: $(ms) $(msold)
	pandoc $(ms) -o $(patsubst %.md, %.tex, $(ms)) $(pflags) --to=latex --standalone
	pandoc $(msold) -o $(patsubst %.md, %.tex, $(msold)) $(pflags) --to=latex --standalone
	latexdiff $(patsubst %.md, %.tex, $(msold)) $(patsubst %.md, %.tex, $(ms)) > $(diff)
	pdflatex $(diff)

resp: $(rsp)
		pandoc $< -o $(patsubst %.md, %.docx, $<) $(pflags)
		pandoc $< -o $(patsubst %.md, %.pdf, $<) $(pflags)

bigpanel:
	convert -append output/fig1P*.png output/figS4_homogenization_panels.png

figS7:
		convert +append output/figS7_ranges.png output/figS7_coldest.png output/figS7.png
