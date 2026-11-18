ROOT=../../..
include $(ROOT)/mk/common.mk

BUILDDIR=./build/obj
RELOC=$(BUILDDIR)/elf.reloc.o
OBJECTS= \
	$(BUILDDIR)/elf.o \
	$(BUILDDIR)/elf-driver.o \
	$(BUILDDIR)/elf-format.o \
	$(BUILDDIR)/elf-loader.o \
	$(BUILDDIR)/elf-segments.o \
	$(BUILDDIR)/elf-server.o \
	$(BUILDDIR)/rose.o \
	$(BUILDDIR)/rose-capabilities.o \
	$(BUILDDIR)/rose-console_io.o \
	$(BUILDDIR)/rose-invocation.o \
	$(BUILDDIR)/rose-interfaces.o \
	$(BUILDDIR)/rose-interfaces-commands-command_endpoint.o \
	$(BUILDDIR)/rose-interfaces-processes-executable.o \
	$(BUILDDIR)/rose-interfaces-processes-executable-server.o \
	$(BUILDDIR)/rose-interfaces-processes-heap-client.o \
	$(BUILDDIR)/rose-interfaces-processes-kernel_process-client.o \
	$(BUILDDIR)/rose-interfaces-processes-memory-client.o \
	$(BUILDDIR)/rose-interfaces-storage-process_banks.o \
	$(BUILDDIR)/rose-interfaces-storage-process_banks-client.o \
	$(BUILDDIR)/rose-interfaces-checkpoints-page_log.o \
	$(BUILDDIR)/rose-interfaces-checkpoints-page_log-client.o \
	$(BUILDDIR)/rose-interfaces-processes-process-client.o \
	$(BUILDDIR)/rose-interfaces-processes-process_control-client.o \
	$(BUILDDIR)/rose-interfaces-storage-bank.o \
	$(BUILDDIR)/rose-interfaces-storage-bank-client.o \
	$(BUILDDIR)/rose-interfaces-storage-dependent.o \
	$(BUILDDIR)/rose-interfaces-storage-dependent-client.o \
	$(BUILDDIR)/rose-interfaces-storage-region.o \
	$(BUILDDIR)/rose-interfaces-storage-region-client.o \
	$(BUILDDIR)/rose-interfaces-storage-region_access-client.o \
	$(BUILDDIR)/rose-interfaces-streams-reader-client.o \
	$(BUILDDIR)/rose-interfaces-streams-writer-client.o \
	$(BUILDDIR)/rose-server.o \
	$(BUILDDIR)/rose-trace.o \
	$(BUILDDIR)/rose-system_calls.o \
	$(BUILDDIR)/rose-system_calls-server.o \
	$(BUILDDIR)/rose-system_calls-client.o \
	$(BUILDDIR)/rose-objects.o \
	$(BUILDDIR)/rose-words.o

all: elf $(RELOC)
	$(GCC) -Wl,-Tlinker.ld  $(GCC_OPTS) -nostdlib -o ../../../build/$(ARCH)/modules/elf $(OBJECTS) ../../../build/$(ARCH)/kernel/obj/util.o -g

elf:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild --RTS=$(KERNEL_RTS) $(GPRBUILD_CARGS)

$(RELOC): $(OBJECTS)
	$(GCC) -Wl,-r -nostdlib $(GCC_OPTS) -o $(RELOC) $(OBJECTS) -g
