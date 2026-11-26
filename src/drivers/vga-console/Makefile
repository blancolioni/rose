ROOT=../../..
include $(ROOT)/mk/common.mk

all: vga_console
	cp ./build/bin/vga-console ../../../build/$(ARCH)/drivers/vga-console

# vga-console is built with the userland run-time system, and serves its
# interfaces through the user-mode IDL target (#367). It uses its own
# Process interface, and so the interfaces that it needs, to keep what is
# typed in unsaved memory (Rose.Unsaved_Memory, #1132).

vga_console:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) \
	    $(INTERFACES)/streams-reader.idl $(INTERFACES)/streams-writer.idl \
	    $(INTERFACES)/streams-stream.idl $(INTERFACES)/screens-terminal.idl \
	    $(INTERFACES)/devices-event_listener.idl $(INTERFACES)/devices-event_source.idl \
	    $(INTERFACES)/processes-restorable.idl \
	    $(INTERFACES)/processes-process.idl \
	    $(INTERFACES)/processes-process_control.idl \
	    $(INTERFACES)/processes-heap.idl $(INTERFACES)/storage-region.idl \
	    $(INTERFACES)/storage-region_access.idl \
	    $(INTERFACES)/storage-bank.idl $(INTERFACES)/storage-dependent.idl \
	    $(INTERFACES)/common-resource.idl
	gprbuild $(GPRBUILD_CARGS)
