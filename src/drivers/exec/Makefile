ROOT=../../..
include $(ROOT)/mk/common.mk

all: exec
	cp ./build/bin/exec ../../../build/$(ARCH)/drivers/exec

# exec is built with the userland run-time system (#490).

exec:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild $(GPRBUILD_CARGS)
