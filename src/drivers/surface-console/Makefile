ROOT=../../..
include $(ROOT)/mk/common.mk

all: surface_console
	cp ./build/bin/surface-console ../../../build/$(ARCH)/drivers/surface-console

# The persistent console on the screen's surface (#653). It is built with
# the userland run-time system, and serves its interfaces through the
# user-mode IDL target, like vga-console. It holds the surface's client,
# and maps its buffer with Rose.Shared_Regions, which needs the Process
# client.

surface_console:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
	    $(INTERFACES)/reader.idl $(INTERFACES)/writer.idl \
	    $(INTERFACES)/stream.idl $(INTERFACES)/terminal.idl \
	    $(INTERFACES)/surface.idl $(INTERFACES)/resource.idl \
	    $(INTERFACES)/region.idl $(INTERFACES)/process.idl \
	    $(INTERFACES)/heap.idl
	gprbuild $(GPRBUILD_CARGS)
