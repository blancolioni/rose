ARCH=i686
BUILDDIR=./build/obj
RELOC=$(BUILDDIR)/ata.reloc.o
GCC=gcc
DEFINES=
GCC_OPTS=-std=gnu99 -ffreestanding -g -finline-limit=3000 -fno-strict-aliasing -Wall -Winline -Wno-format -Wno-char-subscripts -Werror -m32 -nostdinc -nostdlib
UTIL=../../library/util.c
CFILES=$(UTIL)
OBJECTS= \
	$(BUILDDIR)/ada_rts.o \
	$(BUILDDIR)/b__ata-driver.o \
	$(BUILDDIR)/ata-commands.o \
	$(BUILDDIR)/ata-driver.o \
	$(BUILDDIR)/ata-drives.o \
	$(BUILDDIR)/ata.o \
	$(BUILDDIR)/ata-server.o \
	$(BUILDDIR)/rose.o \
	$(BUILDDIR)/rose-addresses.o \
	$(BUILDDIR)/rose-capabilities.o \
	$(BUILDDIR)/rose-console_io.o \
	$(BUILDDIR)/rose-devices.o \
	$(BUILDDIR)/rose-devices-block.o \
	$(BUILDDIR)/rose-devices-block-server.o \
	$(BUILDDIR)/rose-devices-pci.o \
	$(BUILDDIR)/rose-devices-pci-client.o \
	$(BUILDDIR)/rose-devices-port_io.o \
	$(BUILDDIR)/rose-invocation.o \
	$(BUILDDIR)/rose-system_calls.o \
	$(BUILDDIR)/rose-system_calls-client.o \
	$(BUILDDIR)/rose-system_calls-server.o \
	$(BUILDDIR)/rose-objects.o \
	$(BUILDDIR)/rose-words.o

all: ata $(RELOC)
	$(GCC) -Wl,-Tlinker.ld  $(GCC_OPTS) -nostdlib -o ../../../build/$(ARCH)/modules/ata $(OBJECTS) ../../../build/$(ARCH)/kernel/obj/util.o -g

ata:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild --RTS=../../../rts/build/i686

$(RELOC): $(OBJECTS)
	$(GCC) -Wl,-r -nostdlib $(GCC_OPTS) -o $(RELOC) $(OBJECTS) -g

$(BUILDDIR)/ada_rts.o: ../../library/librose/asm/i686/ada_rts.s
	$(GCC) -c -m32 ../../library/librose/asm/i686/ada_rts.s -o $(BUILDDIR)/ada_rts.o
