ARCH=i686
BUILDDIR=./build/obj
RELOC=$(BUILDDIR)/store.reloc.o
GCC=gcc
DEFINES=
GCC_OPTS=-std=gnu99 -ffreestanding -g -finline-limit=3000 -fno-strict-aliasing -Wall -Winline -Wno-format -Wno-char-subscripts -Werror -m32 -nostdinc -nostdlib
UTIL=../../library/util.c
CFILES=$(UTIL)
OBJECTS= \
	$(BUILDDIR)/store.o \
	$(BUILDDIR)/store-driver.o \
	$(BUILDDIR)/store-server.o \
	$(BUILDDIR)/store-devices.o \
	$(BUILDDIR)/rose.o \
	$(BUILDDIR)/rose-capabilities.o \
	$(BUILDDIR)/rose-console_io.o \
	$(BUILDDIR)/rose-invocation.o \
	$(BUILDDIR)/rose-interfaces.o \
	$(BUILDDIR)/rose-interfaces-block_device.o \
	$(BUILDDIR)/rose-interfaces-block_device-client.o \
	$(BUILDDIR)/rose-interfaces-storage.o \
	$(BUILDDIR)/rose-interfaces-storage-server.o \
	$(BUILDDIR)/rose-server.o \
	$(BUILDDIR)/rose-system_calls.o \
	$(BUILDDIR)/rose-system_calls-server.o \
	$(BUILDDIR)/rose-system_calls-client.o \
	$(BUILDDIR)/rose-objects.o \
	$(BUILDDIR)/rose-words.o

all: store $(RELOC)
	$(GCC) -Wl,-Tlinker.ld  $(GCC_OPTS) -nostdlib -o ../../../build/$(ARCH)/modules/store $(OBJECTS) ../../../build/$(ARCH)/kernel/obj/util.o -g

store:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild --RTS=../../../rts/build/i686

$(RELOC): $(OBJECTS)
	$(GCC) -Wl,-r -nostdlib $(GCC_OPTS) -o $(RELOC) $(OBJECTS) -g
