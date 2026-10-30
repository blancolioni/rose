ROSE := $(abspath ../..)
BITLAYOUT := python3 $(ROSE)/scripts/bitlayout.py -o build/layouts
LAYOUT_CHECK := --check \
	--ada-arg=-m32 --ada-arg=--RTS=$(ROSE)/src/rts/build/i686-rose \
	-I $(ROSE)/src/base -I $(ROSE)/src/library/capabilities

PROCESS_DIAGRAM := python3 $(ROSE)/scripts/process-diagram.py -o build/layouts

layouts:
	$(BITLAYOUT) layouts/*.yaml
	$(PROCESS_DIAGRAM) diagrams/processes.yaml

check-layouts:
	$(BITLAYOUT) $(LAYOUT_CHECK) layouts/*.yaml

# The book, as HTML, PDF and EPUB.  rose-book.adoc includes the chapters.
# asciidoctor-epub3 is not packaged by Fedora; install it with
# "gem install --user-install asciidoctor-epub3".
BOOK := rose-book.adoc
ADOC_ATTRS := -a imagesdir=build/layouts

# The HTML (self-contained, with embedded SVGs) is also copied to the
# website, as rose-book.html and index.html.  Run "make adoc PUBLISH_DIR="
# to skip the copy.
PUBLISH_DIR ?= /var/www/rose-os.org/html/rose-book

adoc: layouts
	asciidoctor $(ADOC_ATTRS) -a data-uri -D build $(BOOK)
	asciidoctor-pdf $(ADOC_ATTRS) -D build $(BOOK)
	asciidoctor-epub3 $(ADOC_ATTRS) -D build $(BOOK)
	if [ -n "$(PUBLISH_DIR)" ]; then \
	  cp build/rose-book.html $(PUBLISH_DIR)/; \
	  cp build/rose-book.html $(PUBLISH_DIR)/index.html; \
	  cp build/rose-book.pdf $(PUBLISH_DIR)/; \
	  cp build/rose-book.epub $(PUBLISH_DIR)/; \
	fi

.PHONY: layouts check-layouts adoc

clean:
	rm -rf build
