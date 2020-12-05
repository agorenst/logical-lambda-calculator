SHELL=/bin/bash

files=$(shell for r in `noroots book.nw | grep -v " "`; do echo $${r:2:-2}; done)

tests = $(filter %.tests, $(files))
runtests = $(addsuffix .run, $(tests))

filetests = $(filter %.filetest, $(files))
runfiletests = $(addsuffix .filerun, $(filetests))

$(info files is $(files))
$(info tests is $(tests))
$(info runtests is $(runtests))
$(info runfiletests is $(runfiletests))

go: $(files) $(runtests)

$(files): % : book.nw
	notangle -R$* $^ | cpif $@

%.run : % lambda.pl
	cat $< | while read -r line; do swipl -s lambda.pl -g "$$line" -t halt; done

%.filerun : % lambda.pl
	cat $< | swipl -s lambda.pl -g main

BetaReductionTests.sh: book.nw
	notangle -RBetaReductionTests.sh book.nw > BetaReductionTests.sh
	notangle -R"Beta Reduction Tests" book.nw > "Beta Reduction Tests"
	chmod +777 BetaReductionTests.sh
book: book.pdf
	cp book.pdf /mnt/c/Users/agore/Desktop/prolog_scheme.pdf

book.tex: book.nw
	noweave -latex -n -delay book.nw > book.tex

book.pdf: book.tex
	latexmk -shell-escape -pdf book.tex

clean:
	latexmk -c book.tex -f
	rm -f *.tex *.tests *.filetest *.pl