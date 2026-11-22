ROOT=../../..
include $(ROOT)/mk/common.mk

BUILDDIR=./build/obj
RELOC=$(BUILDDIR)/scan.reloc.o
READER_OBJECTS= \
	$(BUILDDIR)/rose-interfaces-streams-reader.o \
	$(BUILDDIR)/rose-interfaces-streams-reader-client.o
DIRECTORY_OBJECTS= \
	$(BUILDDIR)/rose-interfaces-files-directory.o \
	$(BUILDDIR)/rose-interfaces-files-directory-client.o \
	$(BUILDDIR)/rose-interfaces-common-tree.o \
	$(BUILDDIR)/rose-interfaces-common-tree-client.o \
	$(BUILDDIR)/rose-interfaces-streams-readable.o \
	$(BUILDDIR)/rose-interfaces-streams-readable-client.o
FILE_OBJECTS= \
	$(BUILDDIR)/rose-interfaces-files-file.o \
	$(BUILDDIR)/rose-interfaces-files-file-client.o \
	$(BUILDDIR)/rose-interfaces-streams-releasable_reader.o \
	$(BUILDDIR)/rose-interfaces-streams-releasable_reader-client.o \
	$(BUILDDIR)/rose-interfaces-streams-seekable_reader.o \
	$(BUILDDIR)/rose-interfaces-streams-seekable_reader-client.o
FILE_SYSTEM_OBJECTS= \
	$(BUILDDIR)/rose-interfaces-files-file_system.o \
	$(BUILDDIR)/rose-interfaces-files-file_system-client.o
BLOCK_DEVICE_OBJECTS= \
	$(BUILDDIR)/rose-interfaces-storage-block_array.o \
	$(BUILDDIR)/rose-interfaces-storage-block_array-client.o \
	$(BUILDDIR)/rose-interfaces-storage-block_array-client-table.o

OBJECTS= \
	$(BUILDDIR)/ada_rts.o \
	$(BUILDDIR)/b__scan-driver.o \
	$(BUILDDIR)/scan-driver.o \
	$(BUILDDIR)/scan.o \
	$(BUILDDIR)/rose.o \
	$(BUILDDIR)/rose-capabilities.o \
	$(BUILDDIR)/rose-console_io.o \
	$(BUILDDIR)/rose-devices.o \
	$(BUILDDIR)/rose-devices-gpt.o \
	$(BUILDDIR)/rose-devices-gpt_format.o \
	$(BUILDDIR)/rose-interfaces-streams-writer.o \
	$(BUILDDIR)/rose-interfaces-streams-writer-client.o \
	$(BUILDDIR)/rose-invocation.o \
	$(BUILDDIR)/rose-system_calls.o \
	$(BUILDDIR)/rose-system_calls-client.o \
	$(BUILDDIR)/rose-system_calls-server.o \
	$(BUILDDIR)/rose-objects.o \
	$(BUILDDIR)/rose-words.o \
	$(BLOCK_DEVICE_OBJECTS)

all: scan $(RELOC)
	$(GCC) -Wl,-Tlinker.ld  $(GCC_OPTS) -nostdlib -o ../../../build/$(ARCH)/modules/scan $(OBJECTS) ../../../build/$(ARCH)/kernel/obj/util.o -g

scan:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild --RTS=$(KERNEL_RTS) $(GPRBUILD_CARGS)

$(RELOC): $(OBJECTS)
	$(GCC) -Wl,-r -nostdlib $(GCC_OPTS) -o $(RELOC) $(OBJECTS) -g

$(BUILDDIR)/ada_rts.o: ../../library/asm/i686/ada_rts.s
	$(GCC) -c -m32 ../../library/asm/i686/ada_rts.s -o $(BUILDDIR)/ada_rts.o
