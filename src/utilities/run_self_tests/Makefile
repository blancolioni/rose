ROOT=../../..
include $(ROOT)/mk/common.mk

all: run_self_tests
	cp ./build/bin/run_self_tests-driver ../../../build/$(ARCH)/drivers/run_self_tests

# The spec of Rose.Core.Run_Self_Tests, its main and its metadata are
# generated from IDL (ADR 0002) for the user target (#460, #528), with
# Held_Call, replaceable_test's own test interface (#879). Rose.Core and
# the interfaces the tests use come from the userland library
# (projects/user_library.gpr, #1067).

run_self_tests:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) \
	    rose-core-run_self_tests.idl \
	    ../replaceable_test/held_call.idl
	$(USER_IDLE) --command-client \
	    ../../drivers/file/rose-core-file.idl \
	    ../../drivers/directory/rose-core-directory.idl \
	    ../../drivers/null_stream/rose-core-null_stream.idl
	$(USER_IDLE) --command-client \
	    ../user/rose-core-user.idl \
	    ../auth/rose-core-auth.idl
	gprbuild $(GPRBUILD_CARGS)
