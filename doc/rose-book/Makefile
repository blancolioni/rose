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
ADOC := capabilities.adoc
ADOC_ATTRS := -a imagesdir=build/layouts

# The HTML (self-contained, with embedded SVGs) is also copied to the
# website.  Run "make adoc PUBLISH_DIR=" to skip the copy.
PUBLISH_DIR ?= /var/www/rose-os.org/html/rose-book

# An index page linking each chapter by its title (its "= " line).
build/index.adoc: Makefile $(ADOC)
	mkdir -p build
	{ echo "= The Rose Book"; echo; \
	  for f in $(ADOC); do \
	    title=$$(awk '/^= /{sub(/^= /, ""); print; exit}' $$f); \
	    echo "* link:$${f%.adoc}.html[$$title]"; \
	  done; } > $@

adoc: layouts build/index.adoc
	asciidoctor $(ADOC_ATTRS) -a data-uri -D build $(ADOC)
	asciidoctor -D build build/index.adoc
	if [ -n "$(PUBLISH_DIR)" ]; then \
	  cp build/index.html $(ADOC:%.adoc=build/%.html) $(PUBLISH_DIR)/; \
	fi
	asciidoctor-pdf $(ADOC_ATTRS) -D build $(ADOC)
	asciidoctor-epub3 $(ADOC_ATTRS) -D build $(ADOC)

.PHONY: layouts check-layouts adoc

clean:
	rm -f *.aux *.log *.lof *.idx *.lot *.out *.pdf *.toc
	rm -rf build
