ROOT=../../..
include $(ROOT)/mk/common.mk

all: replaceable_test
	cp ./build/bin/replaceable_test-driver ../../../build/$(ARCH)/drivers/replaceable_test

# The spec of Rose.Core.Replaceable_Test, its main and its metadata are
# generated from IDL (ADR 0002) for the user target, with its own test
# interface, Held_Call (#879). Rose.Core and the interfaces
# replaceable_test uses come from the userland library
# (projects/user_library.gpr, #1067).

replaceable_test:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) rose-core-replaceable_test.idl held_call.idl
	gprbuild $(GPRBUILD_CARGS)
