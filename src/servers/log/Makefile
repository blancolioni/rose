ARCH=i686
BUILDDIR=./build/obj
RELOC=$(BUILDDIR)/log.reloc.o
GCC=gcc
DEFINES=
GCC_OPTS=-std=gnu99 -ffreestanding -g -finline-limit=3000 -fno-strict-aliasing -Wall -Winline -Wno-format -Wno-char-subscripts -Werror -m32 -nostdinc -nostdlib
UTIL=../../library/util.c
CFILES=$(UTIL)
ENVIRONMENT_OBJECTS= \

LIBROSE_OBJECTS=

BLOCK_DEVICE_OBJECTS= \
	$(BUILDDIR)/rose-interfaces-block_device.o \
	$(BUILDDIR)/rose-interfaces-block_device-client.o

KERNEL_LOG_OBJECTS= \
	$(BUILDDIR)/rose-interfaces-kernel_log.o \
	$(BUILDDIR)/rose-interfaces-kernel_log-server.o

STATE_OBJECTS= \
	$(BUILDDIR)/rose-interfaces-state.o \
	$(BUILDDIR)/rose-interfaces-state-server.o

OBJECTS= \
	$(BUILDDIR)/log.o \
	$(BUILDDIR)/log-driver.o \
	$(BUILDDIR)/log-server.o \
	$(BUILDDIR)/rose.o \
	$(BUILDDIR)/rose-capabilities.o \
	$(BUILDDIR)/rose-console_io.o \
	$(BUILDDIR)/rose-interfaces-stream_writer.o \
	$(BUILDDIR)/rose-interfaces-stream_writer-client.o \
	$(BUILDDIR)/rose-invocation.o \
	$(BUILDDIR)/rose-server.o \
	$(BUILDDIR)/rose-system_calls.o \
	$(BUILDDIR)/rose-system_calls-server.o \
	$(BUILDDIR)/rose-system_calls-client.o \
	$(BUILDDIR)/rose-objects.o \
	$(BUILDDIR)/rose-words.o \
	$(ENVIRONMENT_OBJECTS) \
	$(LIBROSE_OBJECTS) \
	$(BLOCK_DEVICE_OBJECTS) \
	$(KERNEL_LOG_OBJECTS) \
	$(STATE_OBJECTS)

all: log $(RELOC)
	$(GCC) -Wl,-Tlinker.ld  $(GCC_OPTS) -nostdlib -o ../../../build/$(ARCH)/modules/log $(OBJECTS) ../../../build/$(ARCH)/kernel/obj/util.o -g

log:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild --RTS=../../../rts/build/i686

$(RELOC): $(OBJECTS)
	$(GCC) -Wl,-r -nostdlib $(GCC_OPTS) -o $(RELOC) $(OBJECTS) -g
