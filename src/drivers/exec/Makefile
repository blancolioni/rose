ROOT=../../..
include $(ROOT)/mk/common.mk

all: exec
	cp ./build/bin/exec ../../../build/$(ARCH)/drivers/exec

# exec is built with the userland run-time system, and serves Exec and
# the installed programs' Launch interfaces through the user-mode IDL
# target (#490). They and the interfaces they use are generated in
# build/generated.

exec:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
	    $(INTERFACES)/exec.idl \
	    $(INTERFACES)/launch.idl \
	    $(INTERFACES)/reader.idl \
	    $(INTERFACES)/writer.idl \
	    $(INTERFACES)/region.idl \
	    $(INTERFACES)/process.idl \
	    $(INTERFACES)/segment.idl \
	    $(INTERFACES)/server.idl \
	    $(INTERFACES)/cap.idl \
	    $(INTERFACES)/heap.idl
	gprbuild $(GPRBUILD_CARGS)
