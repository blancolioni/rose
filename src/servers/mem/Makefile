TARGET=i686-elf
BUILDDIR=./build/obj
DEFINES=
GCC=$(TARGET)-gcc
GPRBUILD=gprbuild
GPRBUILD_ARGS=--compiler-subst=ada,$(GCC) --RTS=/home/fraser/kiln/rose-trunk/rts/build/i686-elf
GCC_OPTS=-std=gnu99 -ffreestanding -g -finline-limit=3000 -fno-strict-aliasing -Wall -Winline -Wno-format -Wno-char-subscripts -Werror -m32 -nostdinc -nostdlib
UTIL=/home/fraser/kiln/rose-trunk/src/library/util.c
CFILES=$(UTIL)
OBJECTS=/home/fraser/kiln/rose-trunk/src/servers/mem/build/obj/mem-calls.o /home/fraser/kiln/rose-trunk/src/servers/mem/build/obj/mem-driver.o /home/fraser/kiln/rose-trunk/src/servers/mem/build/obj/mem.o /home/fraser/kiln/rose-trunk/src/servers/mem/build/obj/mem-physical_map.o /home/fraser/kiln/rose-trunk/src/servers/mem/build/obj/mem-processes.o /home/fraser/kiln/rose-trunk/src/servers/mem/build/obj/mem-server.o /home/fraser/kiln/rose-trunk/src/servers/mem/build/obj/mem-virtual_map.o /home/fraser/kiln/rose-trunk/src/servers/mem/build/obj/rose-addresses.o /home/fraser/kiln/rose-trunk/src/servers/mem/build/obj/rose-capabilities.o /home/fraser/kiln/rose-trunk/src/servers/mem/build/obj/rose-console_io.o /home/fraser/kiln/rose-trunk/src/servers/mem/build/obj/rose-interfaces-memory.o /home/fraser/kiln/rose-trunk/src/servers/mem/build/obj/rose-interfaces-memory-server.o /home/fraser/kiln/rose-trunk/src/servers/mem/build/obj/rose-interfaces.o /home/fraser/kiln/rose-trunk/src/servers/mem/build/obj/rose-interfaces-process-client.o /home/fraser/kiln/rose-trunk/src/servers/mem/build/obj/rose-interfaces-process_memory.o /home/fraser/kiln/rose-trunk/src/servers/mem/build/obj/rose-interfaces-process_memory-server.o /home/fraser/kiln/rose-trunk/src/servers/mem/build/obj/rose-interfaces-process.o /home/fraser/kiln/rose-trunk/src/servers/mem/build/obj/rose-interfaces-region-client.o /home/fraser/kiln/rose-trunk/src/servers/mem/build/obj/rose-interfaces-region.o /home/fraser/kiln/rose-trunk/src/servers/mem/build/obj/rose-interfaces-stream_reader-client.o /home/fraser/kiln/rose-trunk/src/servers/mem/build/obj/rose-interfaces-stream_reader.o /home/fraser/kiln/rose-trunk/src/servers/mem/build/obj/rose-invocation.o /home/fraser/kiln/rose-trunk/src/servers/mem/build/obj/rose-limits.o /home/fraser/kiln/rose-trunk/src/servers/mem/build/obj/rose.o /home/fraser/kiln/rose-trunk/src/servers/mem/build/obj/rose-objects.o /home/fraser/kiln/rose-trunk/src/servers/mem/build/obj/rose-server.o /home/fraser/kiln/rose-trunk/src/servers/mem/build/obj/rose-system_calls-client.o /home/fraser/kiln/rose-trunk/src/servers/mem/build/obj/rose-system_calls.o /home/fraser/kiln/rose-trunk/src/servers/mem/build/obj/rose-system_calls-server.o /home/fraser/kiln/rose-trunk/src/servers/mem/build/obj/rose-words.o
all: mem
	$(GCC) -Wl,-Tlinker.ld  $(GCC_OPTS) -nostdlib -o ../../../build/$(TARGET)/modules/mem $(OBJECTS) /home/fraser/kiln/rose-trunk/build/$(TARGET)/kernel/obj/util.o /home/fraser/kiln/rose-trunk/rts/build/i686-elf/adalib/libgnat.a -g

mem:
	mkdir -p build/obj
	mkdir -p build/bin
	$(GPRBUILD) $(GPRBUILD_ARGS)

$(BUILDDIR)/ada_rts.o: ../../library/asm/i686/ada_rts.s
	$(GCC) -c -m32 ../../library/asm/i686/ada_rts.s -o $(BUILDDIR)/ada_rts.o

