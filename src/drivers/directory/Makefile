ARCH=i686
BUILDDIR=./build/obj
RELOC=$(BUILDDIR)/directory.reloc.o
GCC=gcc
DEFINES=
GCC_OPTS=-std=gnu99 -ffreestanding -g -finline-limit=3000 -fno-strict-aliasing -Wall -Winline -Wno-format -Wno-char-subscripts -Werror -m32 -nostdinc -nostdlib
UTIL=../../library/util.c
CFILES=$(UTIL)
OBJECTS= \
	$(BUILDDIR)/ada_rts.o \
	$(BUILDDIR)/b__directory-driver.o \
	$(BUILDDIR)/directory.o \
	$(BUILDDIR)/directory-driver.o \
	$(BUILDDIR)/directory-server.o \
	$(BUILDDIR)/rose.o \
	$(BUILDDIR)/rose-capabilities.o \
	$(BUILDDIR)/rose-capabilities-standard.o \
	$(BUILDDIR)/rose-interfaces.o \
	$(BUILDDIR)/rose-interfaces-cap_set.o \
	$(BUILDDIR)/rose-interfaces-directory.o \
	$(BUILDDIR)/rose-interfaces-directory-client.o \
	$(BUILDDIR)/rose-interfaces-file.o \
	$(BUILDDIR)/rose-interfaces-file-client.o \
	$(BUILDDIR)/rose-interfaces-heap.o \
	$(BUILDDIR)/rose-interfaces-heap-client.o \
	$(BUILDDIR)/rose-interfaces-launch.o \
	$(BUILDDIR)/rose-interfaces-launch-client.o \
	$(BUILDDIR)/rose-interfaces-process.o \
	$(BUILDDIR)/rose-interfaces-process-client.o \
	$(BUILDDIR)/rose-interfaces-region.o \
	$(BUILDDIR)/rose-interfaces-region-client.o \
	$(BUILDDIR)/rose-interfaces-stream_reader.o \
	$(BUILDDIR)/rose-interfaces-stream_reader-client.o \
	$(BUILDDIR)/rose-interfaces-stream_writer.o \
	$(BUILDDIR)/rose-interfaces-stream_writer-client.o \
	$(BUILDDIR)/rose-interfaces-writable_directory.o \
	$(BUILDDIR)/rose-interfaces-writable_directory-client.o \
	$(BUILDDIR)/rose-interfaces-writable_directory-client-conversions.o \
	$(BUILDDIR)/rose-interfaces-writable_directory-server.o \
	$(BUILDDIR)/rose-interfaces-writable_file.o \
	$(BUILDDIR)/rose-interfaces-writable_file-client.o \
	$(BUILDDIR)/rose-interfaces-file_reader.o \
	$(BUILDDIR)/rose-interfaces-file_reader-client.o \
	$(BUILDDIR)/rose-interfaces-seekable_file_reader.o \
	$(BUILDDIR)/rose-interfaces-seekable_file_reader-client.o \
	$(BUILDDIR)/rose-interfaces-writable_file-client-conversions.o \
	$(BUILDDIR)/rose-invocation.o \
	$(BUILDDIR)/rose-server.o \
	$(BUILDDIR)/rose-system_calls.o \
	$(BUILDDIR)/rose-system_calls-client.o \
	$(BUILDDIR)/rose-system_calls-server.o \
	$(BUILDDIR)/rose-objects.o \
	$(BUILDDIR)/rose-words.o

all: directory $(RELOC)
	$(GCC) -Wl,-Tlinker.ld  $(GCC_OPTS) -nostdlib -o ../../../build/$(ARCH)/drivers/directory $(OBJECTS) ../../../build/$(ARCH)/kernel/obj/util.o -g

directory:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild --RTS=../../../rts/build/i686

$(RELOC): $(OBJECTS)
	$(GCC) -Wl,-r -nostdlib $(GCC_OPTS) -o $(RELOC) $(OBJECTS) -g

$(BUILDDIR)/ada_rts.o: ../../library/asm/i686/ada_rts.s
	$(GCC) -c -m32 ../../library/asm/i686/ada_rts.s -o $(BUILDDIR)/ada_rts.o
