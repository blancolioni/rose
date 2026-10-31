ROOT=../../..
include $(ROOT)/mk/common.mk

BUILDDIR=./build/obj
RELOC=$(BUILDDIR)/elf.reloc.o
OBJECTS= \
	$(BUILDDIR)/elf.o \
	$(BUILDDIR)/elf-driver.o \
	$(BUILDDIR)/elf-format.o \
	$(BUILDDIR)/elf-loader.o \
	$(BUILDDIR)/elf-server.o \
	$(BUILDDIR)/rose.o \
	$(BUILDDIR)/rose-capabilities.o \
	$(BUILDDIR)/rose-console_io.o \
	$(BUILDDIR)/rose-invocation.o \
	$(BUILDDIR)/rose-interfaces.o \
	$(BUILDDIR)/rose-interfaces-block_device.o \
	$(BUILDDIR)/rose-interfaces-block_device-client.o \
	$(BUILDDIR)/rose-interfaces-command_endpoint.o \
	$(BUILDDIR)/rose-interfaces-executable.o \
	$(BUILDDIR)/rose-interfaces-executable-server.o \
	$(BUILDDIR)/rose-interfaces-heap-client.o \
	$(BUILDDIR)/rose-interfaces-kernel_process-client.o \
	$(BUILDDIR)/rose-interfaces-memory-client.o \
	$(BUILDDIR)/rose-interfaces-process-client.o \
	$(BUILDDIR)/rose-interfaces-bank.o \
	$(BUILDDIR)/rose-interfaces-bank-client.o \
	$(BUILDDIR)/rose-interfaces-region.o \
	$(BUILDDIR)/rose-interfaces-region-client.o \
	$(BUILDDIR)/rose-interfaces-storage-client.o \
	$(BUILDDIR)/rose-interfaces-stream_reader-client.o \
	$(BUILDDIR)/rose-interfaces-stream_writer-client.o \
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
	gprbuild --RTS=$(KERNEL_RTS)

$(RELOC): $(OBJECTS)
	$(GCC) -Wl,-r -nostdlib $(GCC_OPTS) -o $(RELOC) $(OBJECTS) -g
