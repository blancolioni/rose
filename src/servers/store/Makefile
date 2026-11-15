ROOT=../../..
include $(ROOT)/mk/common.mk

BUILDDIR=./build/obj
RELOC=$(BUILDDIR)/store.reloc.o
OBJECTS= \
	$(BUILDDIR)/store.o \
	$(BUILDDIR)/store-driver.o \
	$(BUILDDIR)/store-server.o \
	$(BUILDDIR)/store-devices.o \
	$(BUILDDIR)/store-device_pages.o \
	$(BUILDDIR)/store-page_bitmaps.o \
	$(BUILDDIR)/store-page_trees.o \
	$(BUILDDIR)/rose.o \
	$(BUILDDIR)/rose-addresses.o \
	$(BUILDDIR)/rose-checkpoints.o \
	$(BUILDDIR)/rose-capabilities.o \
	$(BUILDDIR)/rose-console_io.o \
	$(BUILDDIR)/rose-invocation.o \
	$(BUILDDIR)/rose-interfaces.o \
	$(BUILDDIR)/rose-interfaces-storage-bank.o \
	$(BUILDDIR)/rose-interfaces-storage-bank-server.o \
	$(BUILDDIR)/rose-interfaces-storage-dependent.o \
	$(BUILDDIR)/rose-interfaces-storage-dependent-server.o \
	$(BUILDDIR)/rose-interfaces-storage-process_banks.o \
	$(BUILDDIR)/rose-interfaces-storage-process_banks-server.o \
	$(BUILDDIR)/rose-interfaces-storage-block_array.o \
	$(BUILDDIR)/rose-interfaces-storage-block_array-client.o \
	$(BUILDDIR)/rose-interfaces-storage-region.o \
	$(BUILDDIR)/rose-interfaces-storage-region-server.o \
	$(BUILDDIR)/rose-interfaces-storage-store.o \
	$(BUILDDIR)/rose-interfaces-storage-store-server.o \
	$(BUILDDIR)/rose-interfaces-checkpoints-storage_checkpoint.o \
	$(BUILDDIR)/rose-interfaces-checkpoints-storage_checkpoint-server.o \
	$(BUILDDIR)/rose-interfaces-streams-reader.o \
	$(BUILDDIR)/rose-interfaces-streams-reader-server.o \
	$(BUILDDIR)/rose-interfaces-streams-writer.o \
	$(BUILDDIR)/rose-interfaces-streams-writer-client.o \
	$(BUILDDIR)/rose-server.o \
	$(BUILDDIR)/rose-trace.o \
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
	gprbuild --RTS=$(KERNEL_RTS) $(GPRBUILD_CARGS)

$(RELOC): $(OBJECTS)
	$(GCC) -Wl,-r -nostdlib $(GCC_OPTS) -o $(RELOC) $(OBJECTS) -g
