ROOT=../../..
include $(ROOT)/mk/common.mk

all: argv
	cp ./build/bin/argv-driver ../../../build/$(ARCH)/drivers/argv

# The spec of Rose.Core.Argv, its main and its metadata, and the package
# Rose.Core, are generated from IDL (ADR 0002), for the user target
# (#371): argv is built with the userland run-time system.

argv:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
	    rose-core-argv.idl $(INTERFACES)/rose-core.idl
	gprbuild $(GPRBUILD_CARGS)
