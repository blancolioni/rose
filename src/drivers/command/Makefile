ROOT=../../..
include $(ROOT)/mk/common.mk

all: command
	cp ./build/bin/command ../../../build/$(ARCH)/drivers/command

# command is built with the userland run-time system (#490).

command:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild
