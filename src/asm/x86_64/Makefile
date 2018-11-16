all: rose

ARCH=x86_64
TARGET=$(ARCH)-unknown-rose
ROSE=../../../build/$(ARCH)/$(TARGET)
BUILDDIR=../../../build/$(ARCH)/kernel/obj
RELOC=$(BUILDDIR)/rose.reloc.o
GCC=gcc
DEFINES=-D__KERNEL__  -DBRING_UP -DDEBUG -DEVT_TRACE
GCC_OPTS=-std=gnu99 -ffreestanding -g -finline-limit=3000 -fno-strict-aliasing -Wall -Winline -Wno-format -Wno-char-subscripts -Werror -m64
INCLUDE=-I./include
UTIL=../../library/util.c
CFILES=$(UTIL)

KERNOBJECTS=\
	$(BUILDDIR)/rose-addresses.o \
	$(BUILDDIR)/rose-boot-sequence.o \
	$(BUILDDIR)/rose-boot-console.o \
	$(BUILDDIR)/rose-boot-memory.o \
	$(BUILDDIR)/rose-main.o \
	$(BUILDDIR)/rose.o \
	$(BUILDDIR)/rose-version.o \
	$(BUILDDIR)/rose-arch.o \
	$(BUILDDIR)/rose-boot.o \
	$(BUILDDIR)/rose-devices.o \
	$(BUILDDIR)/rose-devices-console.o \
	$(BUILDDIR)/rose-kernel.o \
	$(BUILDDIR)/rose-kernel-command_line.o \
	$(BUILDDIR)/rose-kernel-errors.o \
	$(BUILDDIR)/rose-kernel-heap.o \
	$(BUILDDIR)/rose-kernel-init.o \
	$(BUILDDIR)/rose-kernel-modules.o \
	$(BUILDDIR)/rose-kernel-modules-init.o \
	$(BUILDDIR)/rose-kernel-page_table.o \
	$(BUILDDIR)/rose-kernel-panic.o \
	$(BUILDDIR)/rose-kernel-physical_memory.o \
	$(BUILDDIR)/rose-multiboot.o

#FULLKERN=\
	$(BUILDDIR)/rose-boot-console.o \
	$(BUILDDIR)/rose-mkimage.o \
	$(BUILDDIR)/rose-multiboot.o \
	$(BUILDDIR)/rose-bsp-config.o \
	$(BUILDDIR)/rose-bsp.o \
	$(BUILDDIR)/rose-main.o \
	$(BUILDDIR)/rose.o \
	$(BUILDDIR)/rose-version.o \
	$(BUILDDIR)/rose-arch.o \
	$(BUILDDIR)/rose-boot.o \
	$(BUILDDIR)/rose-addresses.o \
	$(BUILDDIR)/rose-boot-init.o \
	$(BUILDDIR)/rose-boot-boot_module_image.o \
	$(BUILDDIR)/rose-capabilities.o \
	$(BUILDDIR)/rose-capabilities-create.o \
	$(BUILDDIR)/rose-devices.o \
	$(BUILDDIR)/rose-devices-console.o \
	$(BUILDDIR)/rose-environment.o \
	$(BUILDDIR)/rose-environment-internal.o \
	$(BUILDDIR)/rose-environment-supplied_environment.o \
	$(BUILDDIR)/rose-formats-elf.o \
	$(BUILDDIR)/rose-kernel.o \
	$(BUILDDIR)/rose-kernel-command_line.o \
	$(BUILDDIR)/rose-kernel-cpu.o \
	$(BUILDDIR)/rose-kernel-errors.o \
	$(BUILDDIR)/rose-kernel-hal.o \
	$(BUILDDIR)/rose-kernel-hal-config.o \
	$(BUILDDIR)/rose-kernel-hal-vm.o \
	$(BUILDDIR)/rose-kernel-hardware_map.o \
	$(BUILDDIR)/rose-kernel-heap.o \
	$(BUILDDIR)/rose-kernel-init.o \
	$(BUILDDIR)/rose-kernel-main.o \
	$(BUILDDIR)/rose-kernel-modules.o \
	$(BUILDDIR)/rose-kernel-modules-init.o \
	$(BUILDDIR)/rose-kernel-mutex.o \
	$(BUILDDIR)/rose-kernel-page_table.o \
	$(BUILDDIR)/rose-kernel-panic.o \
	$(BUILDDIR)/rose-kernel-physical_memory.o \
	$(BUILDDIR)/rose-kernel-processes.o \
	$(BUILDDIR)/rose-kernel-processes-memory.o \
	$(BUILDDIR)/rose-kernel-processes-messages.o \
	$(BUILDDIR)/rose-kernel-syscalls.o

OBJECTS=\
	$(KERNOBJECTS) \
	$(BUILDDIR)/util.o



rose: $(ROSE)

$(ROSE): $(RELOC) linker.ld $(BUILDDIR)/startup.o
	$(GCC) -Wl,-Tlinker.ld  $(GCC_OPTS) -nostdlib -o $(ROSE) $(RELOC)
	size $(ROSE)

$(RELOC): $(OBJECTS)
	$(GCC) -Wl,-r -nostdlib $(GCC_OPTS) -o $(RELOC) $(OBJECTS) -g
	size $(RELOC)

$(BUILDDIR)/startup.o: startup.s
	$(GCC) $(DEFINES) $(GCC_OPTS) -c -o $(BUILDDIR)/startup.o -Iinclude startup.s

$(BUILDDIR)/util.o: $(UTIL)
	$(GCC) $(DEFINES) $(GCC_OPTS) -c -o $(BUILDDIR)/util.o $(UTIL)
