ROOT=../../..
include $(ROOT)/mk/common.mk

BUILDDIR=./build/obj
RELOC=$(BUILDDIR)/log.reloc.o
LIBROSE_OBJECTS=

BLOCK_DEVICE_OBJECTS= \
	$(BUILDDIR)/rose-interfaces-storage-block_array.o \
	$(BUILDDIR)/rose-interfaces-storage-block_array-client.o

KERNEL_LOG_OBJECTS= \
	$(BUILDDIR)/rose-interfaces-checkpoints-kernel_log.o \
	$(BUILDDIR)/rose-interfaces-checkpoints-kernel_log-server.o

LOG_READER_OBJECTS= \
	$(BUILDDIR)/rose-interfaces-checkpoints-log_reader.o \
	$(BUILDDIR)/rose-interfaces-checkpoints-log_reader-server.o

PAGE_LOG_OBJECTS= \
	$(BUILDDIR)/rose-interfaces-checkpoints-page_log.o \
	$(BUILDDIR)/rose-interfaces-checkpoints-page_log-server.o

STATE_OBJECTS= \
	$(BUILDDIR)/rose-interfaces-checkpoints-state.o \
	$(BUILDDIR)/rose-interfaces-checkpoints-state-server.o

OBJECTS= \
	$(BUILDDIR)/log.o \
	$(BUILDDIR)/log-driver.o \
	$(BUILDDIR)/log-server.o \
	$(BUILDDIR)/rose.o \
	$(BUILDDIR)/rose-capabilities.o \
	$(BUILDDIR)/rose-console_io.o \
	$(BUILDDIR)/rose-interfaces-streams-writer.o \
	$(BUILDDIR)/rose-interfaces-streams-writer-client.o \
	$(BUILDDIR)/rose-invocation.o \
	$(BUILDDIR)/rose-server.o \
	$(BUILDDIR)/rose-system_calls.o \
	$(BUILDDIR)/rose-system_calls-server.o \
	$(BUILDDIR)/rose-system_calls-client.o \
	$(BUILDDIR)/rose-objects.o \
	$(BUILDDIR)/rose-words.o \
	$(LIBROSE_OBJECTS) \
	$(BLOCK_DEVICE_OBJECTS) \
	$(KERNEL_LOG_OBJECTS) \
	$(LOG_READER_OBJECTS) \
	$(PAGE_LOG_OBJECTS) \
	$(STATE_OBJECTS)

all: log $(RELOC)
	$(GCC) -Wl,-Tlinker.ld  $(GCC_OPTS) -nostdlib -o ../../../build/$(ARCH)/modules/log $(OBJECTS) ../../../build/$(ARCH)/kernel/obj/util.o -g

log:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild --RTS=$(KERNEL_RTS) $(GPRBUILD_CARGS)

$(RELOC): $(OBJECTS)
	$(GCC) -Wl,-r -nostdlib $(GCC_OPTS) -o $(RELOC) $(OBJECTS) -g
