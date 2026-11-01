ROOT=../../..
include $(ROOT)/mk/common.mk

BUILDDIR=./build/obj
RELOC=$(BUILDDIR)/checkpoint.reloc.o
OBJECTS= \
	$(BUILDDIR)/checkpoint.o \
	$(BUILDDIR)/checkpoint-driver.o \
	$(BUILDDIR)/checkpoint-server.o \
	$(BUILDDIR)/rose.o \
	$(BUILDDIR)/rose-capabilities.o \
	$(BUILDDIR)/rose-console_io.o \
	$(BUILDDIR)/rose-invocation.o \
	$(BUILDDIR)/rose-interfaces.o \
	$(BUILDDIR)/rose-interfaces-cap.o \
	$(BUILDDIR)/rose-interfaces-cap-client.o \
	$(BUILDDIR)/rose-interfaces-stream_writer.o \
	$(BUILDDIR)/rose-interfaces-stream_writer-client.o \
	$(BUILDDIR)/rose-interfaces-timer.o \
	$(BUILDDIR)/rose-interfaces-timer-client.o \
	$(BUILDDIR)/rose-interfaces-checkpointer.o \
	$(BUILDDIR)/rose-interfaces-checkpointer-server.o \
	$(BUILDDIR)/rose-interfaces-timeout.o \
	$(BUILDDIR)/rose-interfaces-timeout-server.o \
	$(BUILDDIR)/rose-server.o \
	$(BUILDDIR)/rose-system_calls.o \
	$(BUILDDIR)/rose-system_calls-server.o \
	$(BUILDDIR)/rose-system_calls-client.o \
	$(BUILDDIR)/rose-objects.o \
	$(BUILDDIR)/rose-words.o

all: checkpoint $(RELOC)
	$(GCC) -Wl,-Tlinker.ld  $(GCC_OPTS) -nostdlib -o ../../../build/$(ARCH)/modules/checkpoint $(OBJECTS) ../../../build/$(ARCH)/kernel/obj/util.o -g

checkpoint:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild --RTS=$(KERNEL_RTS) $(GPRBUILD_CARGS)

$(RELOC): $(OBJECTS)
	$(GCC) -Wl,-r -nostdlib $(GCC_OPTS) -o $(RELOC) $(OBJECTS) -g
