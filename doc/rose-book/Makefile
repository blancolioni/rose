rose-book.pdf: *.tex
	pdflatex rose-book.tex

ROSE := $(abspath ../..)
BITLAYOUT := python3 $(ROSE)/scripts/bitlayout.py -o build/layouts
LAYOUT_CHECK := --check \
	--ada-arg=-m32 --ada-arg=--RTS=$(ROSE)/src/rts/build/i686-rose \
	-I $(ROSE)/src/base -I $(ROSE)/src/library/capabilities

layouts:
	$(BITLAYOUT) layouts/*.yaml

check-layouts:
	$(BITLAYOUT) $(LAYOUT_CHECK) layouts/*.yaml

.PHONY: layouts check-layouts

clean:
	rm -f *.aux *.log *.lof *.idx *.lot *.out *.pdf *.toc
	rm -rf build
