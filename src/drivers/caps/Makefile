ROOT=../../..
include $(ROOT)/mk/common.mk

all: caps
	cp ./build/bin/caps ../../../build/$(ARCH)/drivers/caps

# caps is built with the userland run-time system, and serves its
# interfaces through the user-mode IDL target (#825), whose packages come
# from the userland library (projects/user_library.gpr, #1067).

caps:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild $(GPRBUILD_CARGS)
