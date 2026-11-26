ROOT=../../..
include $(ROOT)/mk/common.mk

all: auth
	cp ./build/bin/auth-driver ../../../build/$(ARCH)/drivers/auth

# The spec of Rose.Core.Auth, its main and its metadata are generated from
# IDL (ADR 0002) for the user target, with the clients of the user and
# directory commands, which New_User launches (#831). Rose.Core and the
# interfaces auth uses come from the userland library
# (projects/user_library.gpr, #1067).

auth:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) rose-core-auth.idl
	$(USER_IDLE) --command-client \
	    ../user/rose-core-user.idl \
	    ../../drivers/directory/rose-core-directory.idl
	gprbuild $(GPRBUILD_CARGS)
