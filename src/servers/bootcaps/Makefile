ROOT=../../..
include $(ROOT)/mk/common.mk

BUILDDIR=./build/obj
RELOC=$(BUILDDIR)/bootcaps.reloc.o

OBJECTS= \
	$(BUILDDIR)/bootcaps.o \
	$(BUILDDIR)/bootcaps-driver.o \
	$(BUILDDIR)/bootcaps-server.o \
	$(BUILDDIR)/rose.o \
	$(BUILDDIR)/rose-capabilities.o \
	$(BUILDDIR)/rose-interfaces-caps-constructor-server.o \
	$(BUILDDIR)/rose-interfaces-caps-cap-server.o \
	$(BUILDDIR)/rose-interfaces-caps-cap_set-server.o \
	$(BUILDDIR)/rose-interfaces-common-resource.o \
	$(BUILDDIR)/rose-invocation.o \
	$(BUILDDIR)/rose-server.o \
	$(BUILDDIR)/rose-system_calls.o \
	$(BUILDDIR)/rose-system_calls-server.o \
	$(BUILDDIR)/rose-system_calls-client.o \
	$(BUILDDIR)/rose-objects.o \
	$(BUILDDIR)/rose-words.o

all: bootcaps $(RELOC)
	$(GCC) -Wl,-Tlinker.ld  $(GCC_OPTS) -nostdlib -o ../../../build/$(ARCH)/modules/bootcaps $(OBJECTS) ../../../build/$(ARCH)/kernel/obj/util.o -g

bootcaps:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild --RTS=$(KERNEL_RTS) $(GPRBUILD_CARGS)

$(RELOC): $(OBJECTS)
	$(GCC) -Wl,-r -nostdlib $(GCC_OPTS) -o $(RELOC) $(OBJECTS) -g
