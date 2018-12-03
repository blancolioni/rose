ARCH=i686
BUILDDIR=./build/obj
RELOC=$(BUILDDIR)/restore.reloc.o
GCC=gcc
DEFINES=
GCC_OPTS=-std=gnu99 -ffreestanding -g -finline-limit=3000 -fno-strict-aliasing -Wall -Winline -Wno-format -Wno-char-subscripts -Werror -m32 -nostdinc -nostdlib
UTIL=../../library/util.c
CFILES=$(UTIL)
STREAM_READER_OBJECTS= \
	$(BUILDDIR)/rose-interfaces-stream_reader.o \
	$(BUILDDIR)/rose-interfaces-stream_reader-client.o
DIRECTORY_OBJECTS= \
	$(BUILDDIR)/rose-interfaces-directory.o \
	$(BUILDDIR)/rose-interfaces-directory-client.o
FILE_OBJECTS= \
	$(BUILDDIR)/rose-interfaces-file.o \
	$(BUILDDIR)/rose-interfaces-file-client.o
FILE_SYSTEM_OBJECTS= \
	$(BUILDDIR)/rose-interfaces-file_system.o \
	$(BUILDDIR)/rose-interfaces-file_system-client.o
BLOCK_DEVICE_OBJECTS= \
	$(BUILDDIR)/rose-interfaces-block_device.o \
	$(BUILDDIR)/rose-interfaces-block_device-client.o

OBJECTS= \
	$(BUILDDIR)/ada_rts.o \
	$(BUILDDIR)/b__restore-driver.o \
	$(BUILDDIR)/restore-driver.o \
	$(BUILDDIR)/restore-installer.o \
	$(BUILDDIR)/restore.o \
	$(BUILDDIR)/rose.o \
	$(BUILDDIR)/rose-capabilities.o \
	$(BUILDDIR)/rose-console_io.o \
	$(BUILDDIR)/rose-directories.o \
	$(BUILDDIR)/rose-devices-checkpoints.o \
	$(BUILDDIR)/rose-environment.o \
	$(BUILDDIR)/rose-environment_pages.o \
	$(BUILDDIR)/rose-interfaces.o \
	$(BUILDDIR)/rose-invocation.o \
	$(BUILDDIR)/rose-system_calls.o \
	$(BUILDDIR)/rose-system_calls-client.o \
	$(BUILDDIR)/rose-system_calls-server.o \
	$(BUILDDIR)/rose-objects.o \
	$(BUILDDIR)/rose-words.o \
	$(DIRECTORY_OBJECTS) \
	$(FILE_OBJECTS) \
	$(FILE_SYSTEM_OBJECTS) \
	$(STREAM_READER_OBJECTS) \
	$(BLOCK_DEVICE_OBJECTS)

all: restore $(RELOC)
	$(GCC) -Wl,-Tlinker.ld  $(GCC_OPTS) -nostdlib -o ../../../build/$(ARCH)/modules/restore $(OBJECTS) ../../../build/$(ARCH)/kernel/obj/util.o -g

restore:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild --RTS=../../../rts/build/i686

$(RELOC): $(OBJECTS)
	$(GCC) -Wl,-r -nostdlib $(GCC_OPTS) -o $(RELOC) $(OBJECTS) -g

$(BUILDDIR)/ada_rts.o: ../../library/asm/i686/ada_rts.s
	$(GCC) -c -m32 ../../library/asm/i686/ada_rts.s -o $(BUILDDIR)/ada_rts.o
