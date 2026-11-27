ROOT=../../..
include $(ROOT)/mk/common.mk

all: keytest
	cp ./build/bin/keytest-driver ../../../build/$(ARCH)/drivers/keytest

# The spec of Rose.Core.Keytest, its main and its metadata are generated
# from IDL (ADR 0002) for the user target: keytest is built with the
# userland run-time system. Rose.Core and the interfaces keytest uses come
# from the userland library (projects/user_library.gpr, #1067).

keytest:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) rose-core-keytest.idl
	gprbuild $(GPRBUILD_CARGS)
