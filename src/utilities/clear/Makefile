ROOT=../../..
include $(ROOT)/mk/common.mk

all: clear
	cp ./build/bin/clear-driver ../../../build/$(ARCH)/drivers/clear

# The spec of Rose.Core.Clear, its main and its metadata are generated
# from IDL (ADR 0002) for the user target. Rose.Core and the interfaces
# clear uses come from the userland library (projects/user_library.gpr,
# #1067).

clear:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) rose-core-clear.idl
	gprbuild $(GPRBUILD_CARGS)
