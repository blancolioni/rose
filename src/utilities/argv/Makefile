ROOT=../../..
include $(ROOT)/mk/common.mk

all: argv
	cp ./build/bin/argv-driver ../../../build/$(ARCH)/drivers/argv

# The spec of Rose.Core.Argv, its main and its metadata are generated from
# IDL (ADR 0002) for the user target (#371): argv is built with the
# userland run-time system. Rose.Core and the interfaces argv uses come
# from the userland library (projects/user_library.gpr, #1067).

argv:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) rose-core-argv.idl
	gprbuild $(GPRBUILD_CARGS)
