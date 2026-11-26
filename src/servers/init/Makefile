ROOT=../../..
include $(ROOT)/mk/common.mk

BUILDDIR=./build/obj
RELOC=$(BUILDDIR)/init.reloc.o
COMMON_OBJECTS= \
	$(BUILDDIR)/ada_rts.o \
	$(BUILDDIR)/init-boot.o \
	$(BUILDDIR)/init-boot_steps.o \
	$(BUILDDIR)/init-calls.o \
	$(BUILDDIR)/init-command_line.o \
	$(BUILDDIR)/init-grants.o \
	$(BUILDDIR)/init-installer.o \
	$(BUILDDIR)/init-run.o \
	$(BUILDDIR)/init-self_tests.o \
	$(BUILDDIR)/init-users.o \
	$(BUILDDIR)/init.o \
	$(BUILDDIR)/rose.o \
	$(BUILDDIR)/rose-capabilities.o \
	$(BUILDDIR)/rose-interfaces.o \
	$(BUILDDIR)/rose-interfaces-users-auth.o \
	$(BUILDDIR)/rose-interfaces-users-auth-client.o \
	$(BUILDDIR)/rose-interfaces-users-auth_admin.o \
	$(BUILDDIR)/rose-interfaces-users-auth_admin-client.o \
	$(BUILDDIR)/rose-interfaces-screens-supervisor.o \
	$(BUILDDIR)/rose-interfaces-screens-supervisor-client.o \
	$(BUILDDIR)/rose-interfaces-storage-bank-client.o \
	$(BUILDDIR)/rose-interfaces-caps-constructor-client.o \
	$(BUILDDIR)/rose-interfaces-caps-releasable_cap_set.o \
	$(BUILDDIR)/rose-interfaces-caps-releasable_cap_set-client.o \
	$(BUILDDIR)/rose-interfaces-processes-replacement-client.o \
	$(BUILDDIR)/rose-interfaces-processes-replacer-client.o \
	$(BUILDDIR)/rose-interfaces-storage-dependent.o \
	$(BUILDDIR)/rose-interfaces-storage-dependent-client.o \
	$(BUILDDIR)/rose-interfaces-storage-block_array-client.o \
	$(BUILDDIR)/rose-interfaces-files-directory-client.o \
	$(BUILDDIR)/rose-interfaces-common-tree.o \
	$(BUILDDIR)/rose-interfaces-common-tree-client.o \
	$(BUILDDIR)/rose-interfaces-streams-readable.o \
	$(BUILDDIR)/rose-interfaces-streams-readable-client.o \
	$(BUILDDIR)/rose-interfaces-files-file-client.o \
	$(BUILDDIR)/rose-interfaces-processes-launch-client.o \
	$(BUILDDIR)/rose-interfaces-storage-process_banks-client.o \
	$(BUILDDIR)/rose-interfaces-common-named-client.o \
	$(BUILDDIR)/rose-interfaces-storage-partitions.o \
	$(BUILDDIR)/rose-interfaces-storage-partitions-client.o \
	$(BUILDDIR)/rose-interfaces-storage-partition_setup.o \
	$(BUILDDIR)/rose-interfaces-storage-partition_setup-client.o \
	$(BUILDDIR)/rose-interfaces-processes-process_control-client.o \
	$(BUILDDIR)/rose-interfaces-processes-starting_process.o \
	$(BUILDDIR)/rose-interfaces-processes-starting_process-client.o \
	$(BUILDDIR)/rose-interfaces-storage-region.o \
	$(BUILDDIR)/rose-interfaces-users-rights.o \
	$(BUILDDIR)/rose-interfaces-users-rights-client.o \
	$(BUILDDIR)/rose-interfaces-users-rights_admin.o \
	$(BUILDDIR)/rose-interfaces-users-rights_admin-client.o \
	$(BUILDDIR)/rose-interfaces-storage-region-client.o \
	$(BUILDDIR)/rose-interfaces-streams-reader.o \
	$(BUILDDIR)/rose-interfaces-storage-store-client.o \
	$(BUILDDIR)/rose-interfaces-screens-terminal.o \
	$(BUILDDIR)/rose-interfaces-screens-terminal-client.o \
	$(BUILDDIR)/rose-interfaces-streams-reader-client.o \
	$(BUILDDIR)/rose-interfaces-users-user.o \
	$(BUILDDIR)/rose-interfaces-users-user-client.o \
	$(BUILDDIR)/rose-interfaces-files-writable_directory-client.o \
	$(BUILDDIR)/rose-interfaces-files-fixed_directory.o \
	$(BUILDDIR)/rose-interfaces-files-fixed_directory-client.o \
	$(BUILDDIR)/rose-interfaces-files-linking_directory.o \
	$(BUILDDIR)/rose-interfaces-files-linking_directory-client.o \
	$(BUILDDIR)/rose-interfaces-files-protecting_directory.o \
	$(BUILDDIR)/rose-interfaces-files-protecting_directory-client.o \
	$(BUILDDIR)/rose-build.o \
	$(BUILDDIR)/rose-version.o \
	$(BUILDDIR)/rose-interfaces-streams-stream.o \
	$(BUILDDIR)/rose-interfaces-streams-stream-client.o \
	$(BUILDDIR)/rose-interfaces-files-writable_file-client.o \
	$(BUILDDIR)/rose-interfaces-files-transactional_writable_file-client.o \
	$(BUILDDIR)/rose-interfaces-common-resource.o \
	$(BUILDDIR)/rose-interfaces-common-resource-client.o \
	$(BUILDDIR)/rose-interfaces-streams-releasable_reader.o \
	$(BUILDDIR)/rose-interfaces-streams-releasable_reader-client.o \
	$(BUILDDIR)/rose-interfaces-streams-releasable_writer.o \
	$(BUILDDIR)/rose-interfaces-streams-releasable_writer-client.o \
	$(BUILDDIR)/rose-interfaces-streams-seekable_reader.o \
	$(BUILDDIR)/rose-interfaces-streams-seekable_reader-client.o \
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
	gprbuild --RTS=$(KERNEL_RTS) $(GPRBUILD_CARGS)

$(RELOC): $(OBJECTS)
	$(GCC) -Wl,-r -nostdlib $(GCC_OPTS) -o $(RELOC) $(OBJECTS) -g

$(BUILDDIR)/ada_rts.o: ../../library/asm/i686/ada_rts.s
	$(GCC) -c -m32 ../../library/asm/i686/ada_rts.s -o $(BUILDDIR)/ada_rts.o
