ROOT=../../..
include $(ROOT)/mk/common.mk

all: process_probe
	cp ./build/bin/process_probe ../../../build/$(ARCH)/drivers/process_probe

# process_probe is a test server that End_Process_Test launches with
# Launch, for its Process cap (#320, #831). It is built with the userland
# run-time system, and serves Named through the user-mode IDL target,
# whose packages come from the userland library
# (projects/user_library.gpr, #1067).

process_probe:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild $(GPRBUILD_CARGS)
