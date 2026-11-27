ROOT=../../..
include $(ROOT)/mk/common.mk

all: keyboard
	cp ./build/bin/keyboard ../../../build/$(ARCH)/drivers/keyboard

# keyboard is built with the userland run-time system, and serves
# Event_Source and its interrupt through the user-mode IDL target (#490).
# They and the interfaces they use come from the userland library
# (projects/user_library.gpr, #1067).

keyboard:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild $(GPRBUILD_CARGS)
