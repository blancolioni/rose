TARGET=i686-elf
BUILDDIR=./build/obj
DEFINES=
GCC=$(TARGET)-gcc
GPRBUILD=gprbuild
GPRBUILD_ARGS=--compiler-subst=ada,$(GCC) --RTS=/home/fraser/kiln/rose-trunk/rts/build/i686-elf
GCC_OPTS=-std=gnu99 -ffreestanding -g -finline-limit=3000 -fno-strict-aliasing -Wall -Winline -Wno-format -Wno-char-subscripts -Werror -m32 -nostdinc -nostdlib
UTIL=/home/fraser/kiln/rose-trunk/src/library/util.c
CFILES=$(UTIL)
OBJECTS=/home/fraser/kiln/rose-trunk/src/drivers/keyboard/build/obj/keyboard-codes.o /home/fraser/kiln/rose-trunk/src/drivers/keyboard/build/obj/keyboard-driver.o /home/fraser/kiln/rose-trunk/src/drivers/keyboard/build/obj/keyboard.o /home/fraser/kiln/rose-trunk/src/drivers/keyboard/build/obj/keyboard-server.o /home/fraser/kiln/rose-trunk/src/drivers/keyboard/build/obj/rose-capabilities.o /home/fraser/kiln/rose-trunk/src/drivers/keyboard/build/obj/rose-devices.o /home/fraser/kiln/rose-trunk/src/drivers/keyboard/build/obj/rose-devices-port_io.o /home/fraser/kiln/rose-trunk/src/drivers/keyboard/build/obj/rose-interfaces-interrupt_handler.o /home/fraser/kiln/rose-trunk/src/drivers/keyboard/build/obj/rose-interfaces.o /home/fraser/kiln/rose-trunk/src/drivers/keyboard/build/obj/rose-interfaces-stream_reader.o /home/fraser/kiln/rose-trunk/src/drivers/keyboard/build/obj/rose-invocation.o /home/fraser/kiln/rose-trunk/src/drivers/keyboard/build/obj/rose.o /home/fraser/kiln/rose-trunk/src/drivers/keyboard/build/obj/rose-objects.o /home/fraser/kiln/rose-trunk/src/drivers/keyboard/build/obj/rose-system_calls-client.o /home/fraser/kiln/rose-trunk/src/drivers/keyboard/build/obj/rose-system_calls.o /home/fraser/kiln/rose-trunk/src/drivers/keyboard/build/obj/rose-system_calls-server.o /home/fraser/kiln/rose-trunk/src/drivers/keyboard/build/obj/rose-words.o
all: keyboard
	$(GCC) -Wl,-Tlinker.ld  $(GCC_OPTS) -nostdlib -o ../../../build/$(TARGET)/drivers/keyboard $(OBJECTS) /home/fraser/kiln/rose-trunk/build/$(TARGET)/kernel/obj/util.o /home/fraser/kiln/rose-trunk/rts/build/i686-elf/adalib/libgnat.a -g

keyboard:
	mkdir -p build/obj
	mkdir -p build/bin
	$(GPRBUILD) $(GPRBUILD_ARGS)

$(BUILDDIR)/ada_rts.o: ../../library/asm/i686/ada_rts.s
	$(GCC) -c -m32 ../../library/asm/i686/ada_rts.s -o $(BUILDDIR)/ada_rts.o

