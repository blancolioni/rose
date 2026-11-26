ROOT=../../..
include $(ROOT)/mk/common.mk

all: list
	cp ./build/bin/list-driver ../../../build/$(ARCH)/drivers/list

# The spec of Rose.Core.List, its main and its metadata are generated from
# IDL (ADR 0002) for the user target. Rose.Core and the interfaces list
# uses come from the userland library (projects/user_library.gpr, #1067).

list:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) rose-core-list.idl
	gprbuild $(GPRBUILD_CARGS)

# Built for the host, to test it without Rose (#1101).
include $(ROOT)/mk/host.mk
