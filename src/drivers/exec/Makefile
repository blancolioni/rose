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
	    $(INTERFACES)/processes-exec.idl \
	    $(INTERFACES)/processes-launch.idl \
	    $(INTERFACES)/streams-reader.idl \
	    $(INTERFACES)/streams-writer.idl \
	    $(INTERFACES)/storage-region.idl \
	    $(INTERFACES)/processes-process.idl \
	    $(INTERFACES)/storage-bank.idl $(INTERFACES)/storage-dependent.idl $(INTERFACES)/common-resource.idl \
	    $(INTERFACES)/processes-segment.idl \
	    $(INTERFACES)/processes-server.idl \
	    $(INTERFACES)/caps-cap.idl \
	    $(INTERFACES)/processes-heap.idl
	gprbuild $(GPRBUILD_CARGS)
