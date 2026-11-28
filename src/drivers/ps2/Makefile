ROOT=../../..
include $(ROOT)/mk/common.mk

all: ps2
	cp ./build/bin/ps2 ../../../build/$(ARCH)/drivers/ps2

# ps2 is built with the userland run-time system, and serves
# Input_Controller and its interrupts through the user-mode IDL target
# (#490, #588). They and the interfaces they use come from the userland
# library (projects/user_library.gpr, #1067).

ps2:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild $(GPRBUILD_CARGS)
