all: rose

ARCH=i686
TARGET=$(ARCH)-unknown-rose
ROSE=../../../build/$(ARCH)/$(TARGET)
BUILDDIR=../../../build/$(ARCH)/kernel/obj
RELOC=$(BUILDDIR)/rose.reloc.o
GCC=gcc
DEFINES=-D__KERNEL__  -DBRING_UP -DDEBUG -DEVT_TRACE
GCC_OPTS=-std=gnu99 -ffreestanding -g -finline-limit=3000 -fno-strict-aliasing -Wall -Winline -Wno-format -Wno-char-subscripts -Werror -m32 -O3
INCLUDE=-I./include
UTIL=../../library/util.c
CFILES=$(UTIL)

KERNOBJECTS=\
	$(BUILDDIR)/rose.o \
	$(BUILDDIR)/rose-addresses.o \
	$(BUILDDIR)/rose-boot-check.o \
	$(BUILDDIR)/rose-boot-console.o \
	$(BUILDDIR)/rose-boot-memory.o \
	$(BUILDDIR)/rose-boot-sequence.o \
	$(BUILDDIR)/rose-capabilities-layout.o \
	$(BUILDDIR)/rose-main.o \
	$(BUILDDIR)/rose-version.o \
	$(BUILDDIR)/rose-arch.o \
	$(BUILDDIR)/rose-arch-gdt.o \
	$(BUILDDIR)/rose-arch-interrupt_table.o \
	$(BUILDDIR)/rose-arch-pic.o \
	$(BUILDDIR)/rose-boot.o \
	$(BUILDDIR)/rose-devices.o \
	$(BUILDDIR)/rose-devices-console.o \
	$(BUILDDIR)/rose-environment_pages.o \
	$(BUILDDIR)/rose-formats.o \
	$(BUILDDIR)/rose-formats-elf.o \
	$(BUILDDIR)/rose-invocation.o \
	$(BUILDDIR)/rose-invocation-trace.o \
	$(BUILDDIR)/rose-kernel.o \
	$(BUILDDIR)/rose-kernel-arch.o \
	$(BUILDDIR)/rose-kernel-capabilities.o \
	$(BUILDDIR)/rose-kernel-capabilities-arch.o \
	$(BUILDDIR)/rose-kernel-capabilities-boot.o \
	$(BUILDDIR)/rose-kernel-capabilities-copy.o \
	$(BUILDDIR)/rose-kernel-capabilities-create.o \
	$(BUILDDIR)/rose-kernel-capabilities-endpoint.o \
	$(BUILDDIR)/rose-kernel-capabilities-interrupt.o \
	$(BUILDDIR)/rose-kernel-capabilities-kernel_caps.o \
	$(BUILDDIR)/rose-kernel-capabilities-meta.o \
	$(BUILDDIR)/rose-kernel-capabilities-pages.o \
	$(BUILDDIR)/rose-kernel-capabilities-page_table.o \
	$(BUILDDIR)/rose-kernel-capabilities-physical_memory.o \
	$(BUILDDIR)/rose-kernel-capabilities-processes.o \
	$(BUILDDIR)/rose-kernel-capabilities-receive.o \
	$(BUILDDIR)/rose-kernel-capabilities-reply.o \
	$(BUILDDIR)/rose-kernel-clock.o \
	$(BUILDDIR)/rose-kernel-command_line.o \
	$(BUILDDIR)/rose-kernel-debug.o \
	$(BUILDDIR)/rose-kernel-errors.o \
	$(BUILDDIR)/rose-kernel-heap.o \
	$(BUILDDIR)/rose-kernel-init.o \
	$(BUILDDIR)/rose-kernel-interrupts.o \
	$(BUILDDIR)/rose-kernel-invocation.o \
	$(BUILDDIR)/rose-kernel-modules.o \
	$(BUILDDIR)/rose-kernel-modules-init.o \
	$(BUILDDIR)/rose-kernel-page_table.o \
	$(BUILDDIR)/rose-kernel-panic.o \
	$(BUILDDIR)/rose-kernel-physical_memory.o \
	$(BUILDDIR)/rose-kernel-processes.o \
	$(BUILDDIR)/rose-kernel-processes-debug.o \
	$(BUILDDIR)/rose-kernel-processes-init.o \
	$(BUILDDIR)/rose-kernel-processes-queue.o \
	$(BUILDDIR)/rose-multiboot.o

OBJECTS=\
	$(KERNOBJECTS) \
	$(BUILDDIR)/util.o \
	$(BUILDDIR)/mp.o


rose: $(ROSE)

$(ROSE): $(RELOC) $(BUILDDIR)/linker.ld $(BUILDDIR)/boot.o
	$(GCC) -Wl,-Tlinker.ld  $(GCC_OPTS) -nostdlib -o $(ROSE) $(RELOC)
	size $(ROSE)

$(RELOC): $(OBJECTS)
	$(GCC) -Wl,-r -nostdlib $(GCC_OPTS) -o $(RELOC) $(OBJECTS) -g
	size $(RELOC)

$(BUILDDIR)/boot.o: boot.s
	$(GCC) $(DEFINES) $(GCC_OPTS) -c -o $(BUILDDIR)/boot.o -Iinclude boot.s

$(BUILDDIR)/mp.o: mp.s
	$(GCC) $(DEFINES) $(GCC_OPTS) -c -o $(BUILDDIR)/mp.o -Iinclude mp.s

$(BUILDDIR)/util.o: $(UTIL)
	$(GCC) $(DEFINES) $(GCC_OPTS) -c -o $(BUILDDIR)/util.o $(UTIL)

$(BUILDDIR)/linker.ld: linker.ld
	cp linker.ld $(BUILDDIR)
