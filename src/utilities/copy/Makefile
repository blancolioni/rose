ROOT=../../..
include $(ROOT)/mk/common.mk

all: copy
	cp ./build/bin/copy-driver ../../../build/$(ARCH)/drivers/copy

# The spec of Rose.Core.Copy, its main and its metadata are generated from
# IDL (ADR 0002) for the user target. Rose.Core and the interfaces copy
# uses come from the userland library (projects/user_library.gpr, #1067).

copy:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) rose-core-copy.idl
	gprbuild $(GPRBUILD_CARGS)

# Built for the host, to test it without Rose (#1101).
include $(ROOT)/mk/host.mk
