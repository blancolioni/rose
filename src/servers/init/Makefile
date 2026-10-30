ARCH=i686
BUILDDIR=./build/obj
RELOC=$(BUILDDIR)/init.reloc.o
GCC=gcc
DEFINES=
GCC_OPTS=-std=gnu99 -ffreestanding -g -finline-limit=3000 -fno-strict-aliasing -Wall -Winline -Wno-format -Wno-char-subscripts -Werror -m32 -nostdinc -nostdlib
UTIL=../../library/util.c
CFILES=$(UTIL)
COMMON_OBJECTS= \
	$(BUILDDIR)/ada_rts.o \
	$(BUILDDIR)/init-boot.o \
	$(BUILDDIR)/init-boot_steps.o \
	$(BUILDDIR)/init-calls.o \
	$(BUILDDIR)/init-clock_checks.o \
	$(BUILDDIR)/init-grants.o \
	$(BUILDDIR)/init-installer.o \
	$(BUILDDIR)/init-run.o \
	$(BUILDDIR)/init-self_tests.o \
	$(BUILDDIR)/init-users.o \
	$(BUILDDIR)/init.o \
	$(BUILDDIR)/rose.o \
	$(BUILDDIR)/rose-capabilities.o \
	$(BUILDDIR)/rose-interfaces.o \
	$(BUILDDIR)/rose-interfaces-auth.o \
	$(BUILDDIR)/rose-interfaces-auth-client.o \
	$(BUILDDIR)/rose-interfaces-auth_admin.o \
	$(BUILDDIR)/rose-interfaces-auth_admin-client.o \
	$(BUILDDIR)/rose-interfaces-clock.o \
	$(BUILDDIR)/rose-interfaces-clock-client.o \
	$(BUILDDIR)/rose-interfaces-directory-client.o \
	$(BUILDDIR)/rose-interfaces-file-client.o \
	$(BUILDDIR)/rose-interfaces-heap-client.o \
	$(BUILDDIR)/rose-interfaces-launch-client.o \
	$(BUILDDIR)/rose-interfaces-partitions.o \
	$(BUILDDIR)/rose-interfaces-partitions-client.o \
	$(BUILDDIR)/rose-interfaces-process-client.o \
	$(BUILDDIR)/rose-interfaces-region.o \
	$(BUILDDIR)/rose-interfaces-region-client.o \
	$(BUILDDIR)/rose-interfaces-stream_reader.o \
	$(BUILDDIR)/rose-interfaces-stream_reader-client.o \
	$(BUILDDIR)/rose-interfaces-user.o \
	$(BUILDDIR)/rose-interfaces-user-client.o \
	$(BUILDDIR)/rose-interfaces-writable_directory-client.o \
	$(BUILDDIR)/rose-interfaces-writable_file-client.o \
	$(BUILDDIR)/rose-interfaces-file_reader.o \
	$(BUILDDIR)/rose-interfaces-file_reader-client.o \
	$(BUILDDIR)/rose-interfaces-seekable_file_reader.o \
	$(BUILDDIR)/rose-interfaces-seekable_file_reader-client.o \
	$(BUILDDIR)/rose-invocation.o \
	$(BUILDDIR)/rose-system_calls.o \
	$(BUILDDIR)/rose-objects.o \
	$(BUILDDIR)/rose-words.o

# init is the boot disk's init, and init-install the install media's
# (#373).  They share everything but their main programs.

OBJECTS= \
	$(BUILDDIR)/b__init-driver.o \
	$(BUILDDIR)/init-driver.o \
	$(COMMON_OBJECTS)

INSTALL_OBJECTS= \
	$(BUILDDIR)/b__init-install_driver.o \
	$(BUILDDIR)/init-install_driver.o \
	$(COMMON_OBJECTS)

all: init $(RELOC)
	$(GCC) -Wl,-Tlinker.ld  $(GCC_OPTS) -nostdlib -o ../../../build/$(ARCH)/modules/init $(OBJECTS) ../../../build/$(ARCH)/kernel/obj/util.o -g
	$(GCC) -Wl,-Tlinker.ld  $(GCC_OPTS) -nostdlib -o ../../../build/$(ARCH)/modules/init-install $(INSTALL_OBJECTS) ../../../build/$(ARCH)/kernel/obj/util.o -g

# Init.Boot, and the GRUB menus' module2 lines, come from boot.yaml
# (#374).

init:
	mkdir -p build/obj
	mkdir -p build/bin
	python3 ../../../scripts/make-init -o build/generated --root ../../.. boot.yaml
	gprbuild --RTS=../../../rts/build/i686

$(RELOC): $(OBJECTS)
	$(GCC) -Wl,-r -nostdlib $(GCC_OPTS) -o $(RELOC) $(OBJECTS) -g

$(BUILDDIR)/ada_rts.o: ../../library/asm/i686/ada_rts.s
	$(GCC) -c -m32 ../../library/asm/i686/ada_rts.s -o $(BUILDDIR)/ada_rts.o
