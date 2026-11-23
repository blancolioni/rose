ROOT=../../..
include $(ROOT)/mk/common.mk

all: vga_console
	cp ./build/bin/vga-console ../../../build/$(ARCH)/drivers/vga-console

# vga-console is built with the userland run-time system, and serves its
# interfaces through the user-mode IDL target (#367).

vga_console:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) \
	    $(INTERFACES)/streams-reader.idl $(INTERFACES)/streams-writer.idl \
	    $(INTERFACES)/streams-stream.idl $(INTERFACES)/screens-terminal.idl \
	    $(INTERFACES)/devices-event_listener.idl $(INTERFACES)/devices-event_source.idl \
	    $(INTERFACES)/processes-restorable.idl
	gprbuild $(GPRBUILD_CARGS)
