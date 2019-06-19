ARCH=i686
BUILDDIR=./build/obj
RELOC=$(BUILDDIR)/caps.reloc.o
GCC=gcc
DEFINES=
GCC_OPTS=-std=gnu99 -ffreestanding -g -finline-limit=3000 -fno-strict-aliasing -Wall -Winline -Wno-format -Wno-char-subscripts -Werror -m32 -nostdinc -nostdlib
UTIL=../../library/util.c
CFILES=$(UTIL)

OBJECTS= \
	$(BUILDDIR)/caps.o \
	$(BUILDDIR)/caps-driver.o \
	$(BUILDDIR)/caps-server.o \
	$(BUILDDIR)/rose.o \
	$(BUILDDIR)/rose-capabilities.o \
	$(BUILDDIR)/rose-console_io.o \
	$(BUILDDIR)/rose-interfaces-constructor-server.o \
	$(BUILDDIR)/rose-interfaces-cap-server.o \
	$(BUILDDIR)/rose-interfaces-cap_set-server.o \
	$(BUILDDIR)/rose-invocation.o \
	$(BUILDDIR)/rose-server.o \
	$(BUILDDIR)/rose-system_calls.o \
	$(BUILDDIR)/rose-system_calls-server.o \
	$(BUILDDIR)/rose-system_calls-client.o \
	$(BUILDDIR)/rose-objects.o \
	$(BUILDDIR)/rose-words.o

all: caps $(RELOC)
	$(GCC) -Wl,-Tlinker.ld  $(GCC_OPTS) -nostdlib -o ../../../build/$(ARCH)/modules/caps $(OBJECTS) ../../../build/$(ARCH)/kernel/obj/util.o -g

caps:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild --RTS=../../../rts/build/i686

$(RELOC): $(OBJECTS)
	$(GCC) -Wl,-r -nostdlib $(GCC_OPTS) -o $(RELOC) $(OBJECTS) -g
