ARCH=i686
BUILDDIR=./build/obj
RELOC=$(BUILDDIR)/command.reloc.o
GCC=gcc
DEFINES=
GCC_OPTS=-std=gnu99 -ffreestanding -g -finline-limit=3000 -fno-strict-aliasing -Wall -Winline -Wno-format -Wno-char-subscripts -Werror -m32 -nostdinc -nostdlib
UTIL=../../library/util.c
CFILES=$(UTIL)
OBJECTS= \
	$(BUILDDIR)/ada_rts.o \
	$(BUILDDIR)/b__command-driver.o \
	$(BUILDDIR)/command.o \
	$(BUILDDIR)/command-driver.o \
	$(BUILDDIR)/command-maps.o \
	$(BUILDDIR)/command-server.o \
	$(BUILDDIR)/rose.o \
	$(BUILDDIR)/rose-capabilities.o \
	$(BUILDDIR)/rose-console_io.o \
	$(BUILDDIR)/rose-interfaces.o \
	$(BUILDDIR)/rose-interfaces-map-server.o \
	$(BUILDDIR)/rose-interfaces-stream_writer-client.o \
	$(BUILDDIR)/rose-invocation.o \
	$(BUILDDIR)/rose-server.o \
	$(BUILDDIR)/rose-system_calls.o \
	$(BUILDDIR)/rose-system_calls-client.o \
	$(BUILDDIR)/rose-system_calls-server.o \
	$(BUILDDIR)/rose-objects.o \
	$(BUILDDIR)/rose-words.o

all: command $(RELOC)
	$(GCC) -Wl,-Tlinker.ld  $(GCC_OPTS) -nostdlib -o ../../../build/$(ARCH)/drivers/command $(OBJECTS) ../../../build/$(ARCH)/kernel/obj/util.o -g

command:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild --RTS=../../../rts/build/i686

$(RELOC): $(OBJECTS)
	$(GCC) -Wl,-r -nostdlib $(GCC_OPTS) -o $(RELOC) $(OBJECTS) -g

$(BUILDDIR)/ada_rts.o: ../../library/asm/i686/ada_rts.s
	$(GCC) -c -m32 ../../library/asm/i686/ada_rts.s -o $(BUILDDIR)/ada_rts.o
