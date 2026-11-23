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
	    $(INTERFACES)/streams-reader.idl $(INTERFACES)/streams-writer.idl \
	    $(INTERFACES)/streams-stream.idl $(INTERFACES)/screens-terminal.idl \
	    $(INTERFACES)/screens-surface.idl $(INTERFACES)/common-resource.idl \
	    $(INTERFACES)/storage-region.idl \
	    $(INTERFACES)/storage-region_access.idl $(INTERFACES)/processes-process.idl \
	    $(INTERFACES)/processes-process_control.idl \
	    $(INTERFACES)/storage-bank.idl $(INTERFACES)/storage-dependent.idl \
	    $(INTERFACES)/processes-heap.idl $(INTERFACES)/processes-restorable.idl
	$(DROP_RTS_UNITS)
	gprbuild $(GPRBUILD_CARGS)
