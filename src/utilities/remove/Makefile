ROOT=../../..
include $(ROOT)/mk/common.mk

all: remove
	cp ./build/bin/remove-driver ../../../build/$(ARCH)/drivers/remove

# The spec of Rose.Core.Remove, its main and its metadata are generated
# from IDL (ADR 0002) for the user target. Rose.Core and the interfaces
# remove uses come from the userland library (projects/user_library.gpr,
# #1067).

remove:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) rose-core-remove.idl
	gprbuild $(GPRBUILD_CARGS)

# Built for the host, to test it without Rose (#1101).
include $(ROOT)/mk/host.mk
