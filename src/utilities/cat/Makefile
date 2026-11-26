ROOT=../../..
include $(ROOT)/mk/common.mk

all: cat
	cp ./build/bin/cat-driver ../../../build/$(ARCH)/drivers/cat

# The spec of Rose.Core.Cat, its main and its metadata are generated from
# IDL (ADR 0002) for the user target: cat is built with the userland
# run-time system. Rose.Core and the interfaces cat uses come from the
# userland library (projects/user_library.gpr, #1067).

cat:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) rose-core-cat.idl
	gprbuild $(GPRBUILD_CARGS)

# Built for the host, to test it without Rose (#1101).
include $(ROOT)/mk/host.mk
