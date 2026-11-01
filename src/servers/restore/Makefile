ROOT=../../..
include $(ROOT)/mk/common.mk

BUILDDIR=./build/obj
RELOC=$(BUILDDIR)/restore.reloc.o
STREAM_READER_OBJECTS= \
	$(BUILDDIR)/rose-interfaces-stream_reader.o \
	$(BUILDDIR)/rose-interfaces-stream_reader-client.o
DIRECTORY_OBJECTS= \
	$(BUILDDIR)/rose-interfaces-directory.o \
	$(BUILDDIR)/rose-interfaces-directory-client.o
FILE_OBJECTS= \
	$(BUILDDIR)/rose-interfaces-file.o \
	$(BUILDDIR)/rose-interfaces-file-client.o \
	$(BUILDDIR)/rose-interfaces-file_reader.o \
	$(BUILDDIR)/rose-interfaces-file_reader-client.o \
	$(BUILDDIR)/rose-interfaces-file_reader-client-conversions.o \
	$(BUILDDIR)/rose-interfaces-resource.o \
	$(BUILDDIR)/rose-interfaces-resource-client.o \
	$(BUILDDIR)/rose-interfaces-seekable_file_reader.o \
	$(BUILDDIR)/rose-interfaces-seekable_file_reader-client.o
FILE_SYSTEM_OBJECTS= \
	$(BUILDDIR)/rose-interfaces-file_system.o \
	$(BUILDDIR)/rose-interfaces-file_system-client.o
BLOCK_DEVICE_OBJECTS= \
	$(BUILDDIR)/rose-interfaces-block_device.o \
	$(BUILDDIR)/rose-interfaces-block_device-client.o
LOG_READER_OBJECTS= \
	$(BUILDDIR)/rose-interfaces-log_reader.o \
	$(BUILDDIR)/rose-interfaces-log_reader-client.o
RESTORE_OBJECTS= \
	$(BUILDDIR)/restore-checkpoints.o \
	$(BUILDDIR)/rose-interfaces-memory_restore.o \
	$(BUILDDIR)/rose-interfaces-memory_restore-client.o \
	$(BUILDDIR)/rose-interfaces-region.o \
	$(BUILDDIR)/rose-interfaces-region-client.o \
	$(BUILDDIR)/rose-interfaces-storage_checkpoint.o \
	$(BUILDDIR)/rose-interfaces-storage_checkpoint-client.o

COMMON_OBJECTS= \
	$(BUILDDIR)/ada_rts.o \
	$(BUILDDIR)/restore-checkpoint_log.o \
	$(BUILDDIR)/restore-start.o \
	$(BUILDDIR)/restore.o \
	$(BUILDDIR)/rose.o \
	$(BUILDDIR)/rose-addresses.o \
	$(BUILDDIR)/rose-capabilities.o \
	$(BUILDDIR)/rose-checkpoints.o \
	$(BUILDDIR)/rose-console_io.o \
	$(BUILDDIR)/rose-interfaces.o \
	$(BUILDDIR)/rose-interfaces-stream_writer.o \
	$(BUILDDIR)/rose-interfaces-stream_writer-client.o \
	$(BUILDDIR)/rose-invocation.o \
	$(BUILDDIR)/rose-system_calls.o \
	$(BUILDDIR)/rose-system_calls-client.o \
	$(BUILDDIR)/rose-system_calls-server.o \
	$(BUILDDIR)/rose-objects.o \
	$(BUILDDIR)/rose-words.o \
	$(LOG_READER_OBJECTS) \
	$(RESTORE_OBJECTS) \
	$(STREAM_READER_OBJECTS)

# restore is the boot disk's restore server, which has no installer, and
# restore-install the install media's (#373).

OBJECTS= \
	$(BUILDDIR)/b__restore-driver.o \
	$(BUILDDIR)/restore-driver.o \
	$(COMMON_OBJECTS)

INSTALL_OBJECTS= \
	$(BUILDDIR)/b__restore-install_driver.o \
	$(BUILDDIR)/restore-install_driver.o \
	$(BUILDDIR)/restore-boot_disk.o \
	$(BUILDDIR)/restore-installer.o \
	$(BUILDDIR)/restore-installer-default.o \
	$(BUILDDIR)/rose-directories.o \
	$(DIRECTORY_OBJECTS) \
	$(FILE_OBJECTS) \
	$(FILE_SYSTEM_OBJECTS) \
	$(BLOCK_DEVICE_OBJECTS) \
	$(COMMON_OBJECTS)

all: restore $(RELOC)
	$(GCC) -Wl,-Tlinker.ld  $(GCC_OPTS) -nostdlib -o ../../../build/$(ARCH)/modules/restore $(OBJECTS) ../../../build/$(ARCH)/kernel/obj/util.o -g
	$(GCC) -Wl,-Tlinker.ld  $(GCC_OPTS) -nostdlib -o ../../../build/$(ARCH)/modules/restore-install $(INSTALL_OBJECTS) ../../../build/$(ARCH)/kernel/obj/util.o -g

restore:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild --RTS=$(KERNEL_RTS) $(GPRBUILD_CARGS)

$(RELOC): $(OBJECTS)
	$(GCC) -Wl,-r -nostdlib $(GCC_OPTS) -o $(RELOC) $(OBJECTS) -g

$(BUILDDIR)/ada_rts.o: ../../library/asm/i686/ada_rts.s
	$(GCC) -c -m32 ../../library/asm/i686/ada_rts.s -o $(BUILDDIR)/ada_rts.o
