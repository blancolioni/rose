ARCH=i686
BUILDDIR=./build/obj
RELOC=$(BUILDDIR)/mem.reloc.o
GCC=gcc
DEFINES=
GCC_OPTS=-std=gnu99 -ffreestanding -g -finline-limit=3000 -fno-strict-aliasing -Wall -Winline -Wno-format -Wno-char-subscripts -Werror -m32 -nostdinc -nostdlib
UTIL=../../library/util.c
CFILES=$(UTIL)
OBJECTS= \
	$(BUILDDIR)/mem.o \
	$(BUILDDIR)/mem-calls.o \
	$(BUILDDIR)/mem-checkpoints.o \
	$(BUILDDIR)/mem-driver.o \
	$(BUILDDIR)/mem-page_table.o \
	$(BUILDDIR)/mem-physical_map.o \
	$(BUILDDIR)/mem-processes.o \
	$(BUILDDIR)/mem-server.o \
	$(BUILDDIR)/mem-virtual_map.o \
	$(BUILDDIR)/rose.o \
	$(BUILDDIR)/rose-addresses.o \
	$(BUILDDIR)/rose-capabilities.o \
	$(BUILDDIR)/rose-console_io.o \
	$(BUILDDIR)/rose-interfaces.o \
	$(BUILDDIR)/rose-interfaces-heap-server.o \
	$(BUILDDIR)/rose-interfaces-memory.o \
	$(BUILDDIR)/rose-interfaces-memory-server.o \
	$(BUILDDIR)/rose-interfaces-process.o \
	$(BUILDDIR)/rose-interfaces-kernel_process-client.o \
	$(BUILDDIR)/rose-interfaces-process-server.o \
	$(BUILDDIR)/rose-interfaces-receiver-server.o \
	$(BUILDDIR)/rose-interfaces-region-client.o \
	$(BUILDDIR)/rose-interfaces-stream_reader.o \
	$(BUILDDIR)/rose-interfaces-stream_reader-client.o \
	$(BUILDDIR)/rose-interfaces-stream_writer.o \
	$(BUILDDIR)/rose-interfaces-stream_writer-client.o \
	$(BUILDDIR)/rose-invocation.o \
	$(BUILDDIR)/rose-server.o \
	$(BUILDDIR)/rose-system_calls.o \
	$(BUILDDIR)/rose-system_calls-server.o \
	$(BUILDDIR)/rose-system_calls-client.o \
	$(BUILDDIR)/rose-objects.o \
	$(BUILDDIR)/rose-words.o

all: mm $(RELOC)
	$(GCC) -Wl,-Tlinker.ld  $(GCC_OPTS) -nostdlib -o ../../../build/$(ARCH)/modules/mem $(OBJECTS) ../../../build/$(ARCH)/kernel/obj/util.o -g

mm:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild --RTS=../../../rts/build/i686

$(RELOC): $(OBJECTS)
	$(GCC) -Wl,-r -nostdlib $(GCC_OPTS) -o $(RELOC) $(OBJECTS) -g
