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
	    $(INTERFACES)/map.idl \
	    $(INTERFACES)/commands.idl \
	    $(INTERFACES)/launch.idl \
	    $(INTERFACES)/process.idl \
	    $(INTERFACES)/bank.idl $(INTERFACES)/dependent.idl $(INTERFACES)/resource.idl $(INTERFACES)/reader.idl \
	    $(INTERFACES)/segment.idl \
	    $(INTERFACES)/server.idl \
	    $(INTERFACES)/cap.idl \
	    $(INTERFACES)/heap.idl \
	    $(INTERFACES)/region.idl
	gprbuild $(GPRBUILD_CARGS)
