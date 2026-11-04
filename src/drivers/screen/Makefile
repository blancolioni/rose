ROOT=../../..
include $(ROOT)/mk/common.mk

all: screen
	cp ./build/bin/screen ../../../build/$(ARCH)/drivers/screen

# The screen driver is built with the userland run-time system, and serves
# its interfaces through the user-mode IDL target, like vga-console. It
# maps its buffer with Rose.Shared_Regions (ADR 0025), which needs the
# Process and Bank clients.

screen:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
	    $(INTERFACES)/surface.idl $(INTERFACES)/resource.idl \
	    $(INTERFACES)/region.idl $(INTERFACES)/stream_reader.idl \
	    $(INTERFACES)/restorable.idl $(INTERFACES)/bank.idl \
	    $(INTERFACES)/process.idl $(INTERFACES)/heap.idl \
	    $(INTERFACES)/event_listener.idl
	gprbuild $(GPRBUILD_CARGS)
