ROOT=../../..
include $(ROOT)/mk/common.mk

all: surface_console
	cp ./build/bin/surface-console ../../../build/$(ARCH)/drivers/surface-console

# The persistent console on the screen's surface (#653). It is built with
# the userland run-time system, and serves its interfaces through the
# user-mode IDL target, like vga-console, whose packages come from the
# userland library (projects/user_library.gpr, #1067). It holds the
# surface's client, and maps its buffer with Rose.Shared_Regions.

surface_console:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild $(GPRBUILD_CARGS)
