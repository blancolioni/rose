ROOT=../../..
include $(ROOT)/mk/common.mk

all: rename
	cp ./build/bin/rename-driver ../../../build/$(ARCH)/drivers/rename

# The spec of Rose.Core.Rename, its main and its metadata are generated
# from IDL (ADR 0002) for the user target. Rose.Core and the interfaces
# rename uses come from the userland library (projects/user_library.gpr,
# #1067).

rename:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) rose-core-rename.idl
	gprbuild $(GPRBUILD_CARGS)

# Built for the host, to test it without Rose (#1101).
include $(ROOT)/mk/host.mk
