TARGET=i686-elf
BUILDDIR=./build/obj
DEFINES=
GCC=$(TARGET)-gcc
GPRBUILD=gprbuild
GPRBUILD_ARGS=--compiler-subst=ada,$(GCC) --RTS=/home/fraser/kiln/rose-trunk/rts/build/i686-elf
GCC_OPTS=-std=gnu99 -ffreestanding -g -finline-limit=3000 -fno-strict-aliasing -Wall -Winline -Wno-format -Wno-char-subscripts -Werror -m32 -nostdinc -nostdlib
UTIL=/home/fraser/kiln/rose-trunk/src/library/util.c
CFILES=$(UTIL)
OBJECTS=/home/fraser/kiln/rose-trunk/src/servers/init/build/obj/init-calls.o /home/fraser/kiln/rose-trunk/src/servers/init/build/obj/init-driver.o /home/fraser/kiln/rose-trunk/src/servers/init/build/obj/init-installer.o /home/fraser/kiln/rose-trunk/src/servers/init/build/obj/init.o /home/fraser/kiln/rose-trunk/src/servers/init/build/obj/init-run.o /home/fraser/kiln/rose-trunk/src/servers/init/build/obj/rose-capabilities.o /home/fraser/kiln/rose-trunk/src/servers/init/build/obj/rose-devices.o /home/fraser/kiln/rose-trunk/src/servers/init/build/obj/rose-devices-partitions.o /home/fraser/kiln/rose-trunk/src/servers/init/build/obj/rose-interfaces-block_device.o /home/fraser/kiln/rose-trunk/src/servers/init/build/obj/rose-interfaces-constructor.o /home/fraser/kiln/rose-trunk/src/servers/init/build/obj/rose-interfaces-exec.o /home/fraser/kiln/rose-trunk/src/servers/init/build/obj/rose-interfaces-executable.o /home/fraser/kiln/rose-trunk/src/servers/init/build/obj/rose-interfaces-file_system.o /home/fraser/kiln/rose-trunk/src/servers/init/build/obj/rose-interfaces-installer.o /home/fraser/kiln/rose-trunk/src/servers/init/build/obj/rose-interfaces-memory.o /home/fraser/kiln/rose-trunk/src/servers/init/build/obj/rose-interfaces.o /home/fraser/kiln/rose-trunk/src/servers/init/build/obj/rose-interfaces-partitions-client.o /home/fraser/kiln/rose-trunk/src/servers/init/build/obj/rose-interfaces-partitions.o /home/fraser/kiln/rose-trunk/src/servers/init/build/obj/rose-interfaces-region-client.o /home/fraser/kiln/rose-trunk/src/servers/init/build/obj/rose-interfaces-region.o /home/fraser/kiln/rose-trunk/src/servers/init/build/obj/rose-interfaces-storage.o /home/fraser/kiln/rose-trunk/src/servers/init/build/obj/rose-interfaces-stream_reader-client.o /home/fraser/kiln/rose-trunk/src/servers/init/build/obj/rose-interfaces-stream_reader.o /home/fraser/kiln/rose-trunk/src/servers/init/build/obj/rose-interfaces-timeout.o /home/fraser/kiln/rose-trunk/src/servers/init/build/obj/rose-interfaces-timer.o /home/fraser/kiln/rose-trunk/src/servers/init/build/obj/rose-invocation.o /home/fraser/kiln/rose-trunk/src/servers/init/build/obj/rose-limits.o /home/fraser/kiln/rose-trunk/src/servers/init/build/obj/rose.o /home/fraser/kiln/rose-trunk/src/servers/init/build/obj/rose-objects.o /home/fraser/kiln/rose-trunk/src/servers/init/build/obj/rose-system_calls.o /home/fraser/kiln/rose-trunk/src/servers/init/build/obj/rose-words.o
all: init
	$(GCC) -Wl,-Tlinker.ld  $(GCC_OPTS) -nostdlib -o ../../../build/$(TARGET)/modules/init $(OBJECTS) /home/fraser/kiln/rose-trunk/build/$(TARGET)/kernel/obj/util.o /home/fraser/kiln/rose-trunk/rts/build/i686-elf/adalib/libgnat.a -g

init:
	mkdir -p build/obj
	mkdir -p build/bin
	$(GPRBUILD) $(GPRBUILD_ARGS)

$(BUILDDIR)/ada_rts.o: ../../library/asm/i686/ada_rts.s
	$(GCC) -c -m32 ../../library/asm/i686/ada_rts.s -o $(BUILDDIR)/ada_rts.o

