ROOT=../../..
include $(ROOT)/mk/common.mk

all: touch
	cp ./build/bin/touch-driver ../../../build/$(ARCH)/drivers/touch

# The spec of Rose.Core.Touch, its main and its metadata are generated
# from IDL (ADR 0002) for the user target. Rose.Core and the interfaces
# touch uses come from the userland library (projects/user_library.gpr,
# #1067).

touch:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) rose-core-touch.idl
	gprbuild $(GPRBUILD_CARGS)

# Built for the host, to test it without Rose (#1101).
include $(ROOT)/mk/host.mk
