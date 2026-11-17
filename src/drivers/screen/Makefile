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
	    $(INTERFACES)/screens-surface.idl $(INTERFACES)/common-resource.idl \
	    $(INTERFACES)/storage-region.idl $(INTERFACES)/streams-reader.idl \
	    $(INTERFACES)/processes-restorable.idl $(INTERFACES)/storage-bank.idl \
	    $(INTERFACES)/storage-dependent.idl \
	    $(INTERFACES)/processes-process.idl $(INTERFACES)/processes-heap.idl \
	    $(INTERFACES)/processes-process_control.idl \
	    $(INTERFACES)/devices-event_listener.idl $(INTERFACES)/devices-event_source.idl
	gprbuild $(GPRBUILD_CARGS)
