ARCH=i686
BUILDDIR=./build/obj
RELOC=$(BUILDDIR)/init.reloc.o
GCC=gcc
DEFINES=
GCC_OPTS=-std=gnu99 -ffreestanding -g -finline-limit=3000 -fno-strict-aliasing -Wall -Winline -Wno-format -Wno-char-subscripts -Werror -m32 -nostdinc -nostdlib
UTIL=../../library/util.c
CFILES=$(UTIL)
OBJECTS= \
	$(BUILDDIR)/ada_rts.o \
	$(BUILDDIR)/b__init-driver.o \
	$(BUILDDIR)/init-calls.o \
	$(BUILDDIR)/init-driver.o \
	$(BUILDDIR)/init-installer.o \
	$(BUILDDIR)/init-run.o \
	$(BUILDDIR)/init.o \
	$(BUILDDIR)/rose.o \
	$(BUILDDIR)/rose-capabilities.o \
	$(BUILDDIR)/rose-environment.o \
	$(BUILDDIR)/rose-environment_pages.o \
	$(BUILDDIR)/rose-interfaces.o \
	$(BUILDDIR)/rose-interfaces-partitions.o \
	$(BUILDDIR)/rose-interfaces-partitions-client.o \
	$(BUILDDIR)/rose-interfaces-region.o \
	$(BUILDDIR)/rose-interfaces-region-client.o \
	$(BUILDDIR)/rose-interfaces-stream_reader.o \
	$(BUILDDIR)/rose-interfaces-stream_reader-client.o \
	$(BUILDDIR)/rose-invocation.o \
	$(BUILDDIR)/rose-system_calls.o \
	$(BUILDDIR)/rose-objects.o \
	$(BUILDDIR)/rose-words.o

# all: src/init-script.ads init $(RELOC)
all: init $(RELOC)
	$(GCC) -Wl,-Tlinker.ld  $(GCC_OPTS) -nostdlib -o ../../../build/$(ARCH)/modules/init $(OBJECTS) ../../../build/$(ARCH)/kernel/obj/util.o -g

#src/init-script.ads: ../../../scripts/petal/init.petal ../../../scripts/petal/aquarius/petal/aqua/*.aqua
#	(cd src; aquarius --plugin-dir=../../../../scripts/petal/aquarius -f -i ../../../../scripts/petal/init.petal -a init)

init:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild --RTS=../../../rts/build/i686

$(RELOC): $(OBJECTS)
	$(GCC) -Wl,-r -nostdlib $(GCC_OPTS) -o $(RELOC) $(OBJECTS) -g

$(BUILDDIR)/ada_rts.o: ../../library/asm/i686/ada_rts.s
	$(GCC) -c -m32 ../../library/asm/i686/ada_rts.s -o $(BUILDDIR)/ada_rts.o
