ROOT=../../..
include $(ROOT)/mk/common.mk

all: keyboard
	cp ./build/bin/keyboard ../../../build/$(ARCH)/drivers/keyboard

# keyboard is built with the userland run-time system (#490).

keyboard:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild
