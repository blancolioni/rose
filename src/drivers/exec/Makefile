ROOT=../../..
include $(ROOT)/mk/common.mk

all: exec
	cp ./build/bin/exec ../../../build/$(ARCH)/drivers/exec

# exec is built with the userland run-time system, and serves Exec and the
# installed programs' Launch interfaces through the user-mode IDL target
# (#490). They and the interfaces they use come from the userland library
# (projects/user_library.gpr, #1067).

exec:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild $(GPRBUILD_CARGS)
