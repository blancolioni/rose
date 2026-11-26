ROOT=../../..
include $(ROOT)/mk/common.mk

all: screen
	cp ./build/bin/screen ../../../build/$(ARCH)/drivers/screen

# The screen driver is built with the userland run-time system, and serves
# its interfaces through the user-mode IDL target, like vga-console, whose
# packages come from the userland library (projects/user_library.gpr,
# #1067). It maps its buffer with Rose.Shared_Regions (ADR 0025).

screen:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild $(GPRBUILD_CARGS)
