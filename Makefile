SHELL=/bin/bash

files=$(shell for r in `noroots book.nw | grep -v " "`; do echo $${r:2:-2}; done)

tests = $(filter %.tests, $(files))
runtests = $(addsuffix .run, $(tests))

filetests = $(filter %.filetest, $(files))
runfiletests = $(addsuffix .filerun, $(filetests))

go: $(files) $(runtests) $(runfiletests)

$(files): % : book.nw
	notangle -R$* $^ | cpif $@
book.tex: book.nw $(files)
	./myweave.sh > book.tex
#noweave -latex -n -delay book.nw > book.tex

%.run : % lambda.pl
	cat $< | while read -r line; do swipl -s lambda.pl -g "$$line" -t halt; done

%.filerun : % lambda.pl
	cat $< | swipl -s lambda.pl -g main

book: book.pdf
	cp book.pdf /mnt/c/Users/agore/Desktop/prolog_scheme.pdf


book.pdf: book.tex mynoweb.py
	latexmk -shell-escape -pdf book.tex

clean:
	latexmk -c book.tex -f
	rm -f *.tex *.tests *.filetest *.pl
