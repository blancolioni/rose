ARCH=i686
BUILDDIR=./build/obj
RELOC=$(BUILDDIR)/launch.reloc.o
GCC=gcc
DEFINES=
GCC_OPTS=-std=gnu99 -ffreestanding -g -finline-limit=3000 -fno-strict-aliasing -Wall -Winline -Wno-format -Wno-char-subscripts -Werror -m32 -nostdinc -nostdlib
UTIL=../../library/util.c
CFILES=$(UTIL)
ENVIRONMENT_OBJECTS= \
	$(BUILDDIR)/rose-environment.o \
	$(BUILDDIR)/rose-environment_pages.o \
	$(BUILDDIR)/rose-command_line.o

LIBROSE_OBJECTS=$(BUILDDIR)/rose-strings.o
BLOCK_DEVICE_OBJECTS= \
	$(BUILDDIR)/rose-interfaces-block_device.o \
	$(BUILDDIR)/rose-interfaces-block_device-client.o

OBJECTS= \
	$(BUILDDIR)/launch.o \
	$(BUILDDIR)/launch-driver.o \
	$(BUILDDIR)/launch-server.o \
	$(BUILDDIR)/rose.o \
	$(BUILDDIR)/rose-capabilities.o \
	$(BUILDDIR)/rose-console_io.o \
	$(BUILDDIR)/rose-interfaces.o \
	$(BUILDDIR)/rose-interfaces-launch.o \
	$(BUILDDIR)/rose-interfaces-launch-server.o \
	$(BUILDDIR)/rose-invocation.o \
	$(BUILDDIR)/rose-server.o \
	$(BUILDDIR)/rose-system_calls.o \
	$(BUILDDIR)/rose-system_calls-server.o \
	$(BUILDDIR)/rose-system_calls-client.o \
	$(BUILDDIR)/rose-objects.o \
	$(BUILDDIR)/rose-words.o \
	$(ENVIRONMENT_OBJECTS) \
	$(LIBROSE_OBJECTS) \
	$(BLOCK_DEVICE_OBJECTS)

all: launch $(RELOC)
	$(GCC) -Wl,-Tlinker.ld  $(GCC_OPTS) -nostdlib -o ../../../build/$(ARCH)/modules/launch $(OBJECTS) ../../../build/$(ARCH)/kernel/obj/util.o -g

launch:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild --RTS=../../../rts/build/i686

$(RELOC): $(OBJECTS)
	$(GCC) -Wl,-r -nostdlib $(GCC_OPTS) -o $(RELOC) $(OBJECTS) -g
