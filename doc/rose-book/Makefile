ROSE := $(abspath ../..)
BITLAYOUT := python3 $(ROSE)/scripts/bitlayout.py -o build/layouts
LAYOUT_CHECK := --check \
	--ada-arg=-m32 --ada-arg=--RTS=$(ROSE)/rts/user/build/i686-rose \
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

# This only builds the book.  The website's copy is published from the
# main branch by the rose-book timer in the rose-os.org repository.

adoc: layouts
	asciidoctor $(ADOC_ATTRS) -a data-uri -D build $(BOOK)
	asciidoctor-pdf $(ADOC_ATTRS) -D build $(BOOK)
	asciidoctor-epub3 $(ADOC_ATTRS) -D build $(BOOK)

.PHONY: layouts check-layouts adoc

clean:
	rm -rf build
