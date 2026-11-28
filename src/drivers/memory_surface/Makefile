ROOT=../../..
include $(ROOT)/mk/common.mk

all: memory_surface
	cp ./build/bin/memory_surface ../../../build/$(ARCH)/drivers/memory_surface

# memory_surface is a test server: a Test_Surface in plain memory, which
# the display server's tests launch with Launch as a stand-in for the
# screen (#583). It is built with the userland run-time system, and
# serves through the user-mode IDL target, whose packages come from the
# userland library (projects/user_library.gpr, #1067).

memory_surface:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild $(GPRBUILD_CARGS)
