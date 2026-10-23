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

# AsciiDoc trial: the chapters converted so far, as HTML, PDF and EPUB.
# asciidoctor-epub3 is not packaged by Fedora; install it with
# "gem install --user-install asciidoctor-epub3".
ADOC := capabilities.adoc idl.adoc
ADOC_ATTRS := -a imagesdir=build/layouts

adoc: layouts
	asciidoctor $(ADOC_ATTRS) -a data-uri -D build $(ADOC)
	asciidoctor-pdf $(ADOC_ATTRS) -D build $(ADOC)
	asciidoctor-epub3 $(ADOC_ATTRS) -D build $(ADOC)

.PHONY: layouts check-layouts adoc

clean:
	rm -f *.aux *.log *.lof *.idx *.lot *.out *.pdf *.toc
	rm -rf build
