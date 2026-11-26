ROOT=../../..
include $(ROOT)/mk/common.mk

all: hold_home
	cp ./build/bin/hold_home-driver ../../../build/$(ARCH)/drivers/hold_home

# The spec of Rose.Core.Hold_Home, its main and its metadata are generated
# from IDL (ADR 0002) for the user target, with the clients of the user
# and directory commands, which it launches (#831, idle#103). Rose.Core
# and the interfaces hold_home uses come from the userland library
# (projects/user_library.gpr, #1067).

hold_home:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) rose-core-hold_home.idl
	$(USER_IDLE) --command-client \
	    ../user/rose-core-user.idl \
	    ../../drivers/directory/rose-core-directory.idl
	gprbuild $(GPRBUILD_CARGS)
