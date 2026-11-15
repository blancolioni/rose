ROOT=../../..
include $(ROOT)/mk/common.mk

all: command
	cp ./build/bin/command ../../../build/$(ARCH)/drivers/command

# command is built with the userland run-time system, and serves the
# registry through the user-mode IDL target (#490). Map and the
# interfaces it uses are generated in build/generated.

command:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
	    $(INTERFACES)/commands-map.idl \
	    $(INTERFACES)/commands-lookup.idl \
	    $(INTERFACES)/processes-launch.idl \
	    $(INTERFACES)/processes-process.idl \
	    $(INTERFACES)/storage-bank.idl $(INTERFACES)/storage-dependent.idl $(INTERFACES)/common-resource.idl $(INTERFACES)/streams-reader.idl \
	    $(INTERFACES)/processes-segment.idl \
	    $(INTERFACES)/processes-server.idl \
	    $(INTERFACES)/caps-cap.idl \
	    $(INTERFACES)/processes-heap.idl \
	    $(INTERFACES)/storage-region.idl
	gprbuild $(GPRBUILD_CARGS)
