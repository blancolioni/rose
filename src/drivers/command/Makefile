ROOT=../../..
include $(ROOT)/mk/common.mk

all: command
	cp ./build/bin/command ../../../build/$(ARCH)/drivers/command

# command is built with the userland run-time system, and serves the
# registry through the user-mode IDL target (#490). Map and the interfaces
# it uses come from the userland library (projects/user_library.gpr,
# #1067).

command:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild $(GPRBUILD_CARGS)
