ARCH=i686
BUILDDIR=./build/obj

RELOC=$(BUILDDIR)/keyboard.reloc.o
all: config hello

config:
	mkdir -p build/bin
	mkdir -p build/obj


hello: build/bin/hello
	cp build/bin/hello ../../../build/$(ARCH)/drivers

build/bin/hello:
	gprbuild hello.gpr
