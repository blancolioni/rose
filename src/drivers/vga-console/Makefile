ARCH=i686
IDLE ?= idle
INTERFACES=../../../interfaces

all: vga_console
	cp ./build/bin/vga-console ../../../build/$(ARCH)/drivers/vga-console

# vga-console is built with the userland run-time system, and serves its
# interfaces through the user-mode IDL target (#367).

vga_console:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target user -o build/generated \
	    $(INTERFACES)/stream_writer.idl $(INTERFACES)/restorable.idl
	gprbuild
