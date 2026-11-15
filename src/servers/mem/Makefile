ROOT=../../..
include $(ROOT)/mk/common.mk

BUILDDIR=./build/obj
RELOC=$(BUILDDIR)/mem.reloc.o
OBJECTS= \
	$(BUILDDIR)/mem.o \
	$(BUILDDIR)/mem-calls.o \
	$(BUILDDIR)/mem-checkpoints.o \
	$(BUILDDIR)/mem-driver.o \
	$(BUILDDIR)/mem-page_cache.o \
	$(BUILDDIR)/mem-page_table.o \
	$(BUILDDIR)/mem-physical_map.o \
	$(BUILDDIR)/mem-processes.o \
	$(BUILDDIR)/mem-server.o \
	$(BUILDDIR)/mem-shared_pages.o \
	$(BUILDDIR)/mem-virtual_map.o \
	$(BUILDDIR)/rose.o \
	$(BUILDDIR)/rose-addresses.o \
	$(BUILDDIR)/rose-capabilities.o \
	$(BUILDDIR)/rose-console_io.o \
	$(BUILDDIR)/rose-interfaces.o \
	$(BUILDDIR)/rose-interfaces-processes-heap-server.o \
	$(BUILDDIR)/rose-interfaces-processes-memory.o \
	$(BUILDDIR)/rose-interfaces-processes-memory-server.o \
	$(BUILDDIR)/rose-interfaces-checkpoints-memory_restore.o \
	$(BUILDDIR)/rose-interfaces-checkpoints-memory_restore-server.o \
	$(BUILDDIR)/rose-interfaces-checkpoints-page_log.o \
	$(BUILDDIR)/rose-interfaces-checkpoints-page_log-client.o \
	$(BUILDDIR)/rose-interfaces-processes-process.o \
	$(BUILDDIR)/rose-interfaces-processes-kernel_process-client.o \
	$(BUILDDIR)/rose-interfaces-processes-process-server.o \
	$(BUILDDIR)/rose-interfaces-caps-receiver-server.o \
	$(BUILDDIR)/rose-interfaces-processes-replacement.o \
	$(BUILDDIR)/rose-interfaces-processes-replacement-server.o \
	$(BUILDDIR)/rose-interfaces-processes-replacer.o \
	$(BUILDDIR)/rose-interfaces-processes-replacer-server.o \
	$(BUILDDIR)/rose-interfaces-storage-region-client.o \
	$(BUILDDIR)/rose-interfaces-storage-bank.o \
	$(BUILDDIR)/rose-interfaces-storage-bank-client.o \
	$(BUILDDIR)/rose-interfaces-storage-dependent.o \
	$(BUILDDIR)/rose-interfaces-storage-dependent-client.o \
	$(BUILDDIR)/rose-interfaces-storage-process_banks.o \
	$(BUILDDIR)/rose-interfaces-storage-process_banks-client.o \
	$(BUILDDIR)/rose-interfaces-streams-reader.o \
	$(BUILDDIR)/rose-interfaces-streams-reader-client.o \
	$(BUILDDIR)/rose-interfaces-streams-writer.o \
	$(BUILDDIR)/rose-interfaces-streams-writer-client.o \
	$(BUILDDIR)/rose-invocation.o \
	$(BUILDDIR)/rose-server.o \
	$(BUILDDIR)/rose-trace.o \
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
	gprbuild --RTS=$(KERNEL_RTS) $(GPRBUILD_CARGS)

$(RELOC): $(OBJECTS)
	$(GCC) -Wl,-r -nostdlib $(GCC_OPTS) -o $(RELOC) $(OBJECTS) -g
