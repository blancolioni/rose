rose-book.pdf: *.tex
	pdflatex rose-book.tex

layouts: layouts/*.yaml ../../scripts/bitlayout.py
	python3 ../../scripts/bitlayout.py -o build/layouts layouts/*.yaml

.PHONY: layouts

clean:
	rm -f *.aux *.log *.lof *.idx *.lot *.out *.pdf *.toc
	rm -rf build
