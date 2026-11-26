ROOT=../../..
include $(ROOT)/mk/common.mk

all: user
	cp ./build/bin/user-driver ../../../build/$(ARCH)/drivers/user

# The spec of Rose.Core.User, its main and its metadata are generated from
# IDL (ADR 0002) for the user target. It is a replaceable server (#831).
# Rose.Core and the interfaces user uses come from the userland library
# (projects/user_library.gpr, #1067).

user:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) rose-core-user.idl
	gprbuild $(GPRBUILD_CARGS)
