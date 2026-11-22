ROOT=../../..
include $(ROOT)/mk/common.mk

all: run_self_tests
	cp ./build/bin/run_self_tests-driver ../../../build/$(ARCH)/drivers/run_self_tests

# The spec of Rose.Core.Run_Self_Tests, its main and its metadata, and the
# package Rose.Core, and the interfaces that the tests use with their
# parents, are generated from IDL (ADR 0002, #460) for the user target
# (#528). Held_Call is replaceable_test's own test interface (#879).

run_self_tests:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
	    rose-core-run_self_tests.idl $(INTERFACES)/rose-core.idl \
	    $(INTERFACES)/commands-lookup.idl \
	    $(INTERFACES)/users-auth.idl \
	    $(INTERFACES)/users-auth_admin.idl \
	    $(INTERFACES)/time-clock.idl \
	    $(INTERFACES)/users-rights.idl \
	    $(INTERFACES)/users-rights_admin.idl \
	    $(INTERFACES)/processes-launch.idl \
	    $(INTERFACES)/users-user.idl \
	    $(INTERFACES)/common-named.idl \
	    $(INTERFACES)/files-writable_directory.idl \
	    $(INTERFACES)/files-fixed_directory.idl \
	    $(INTERFACES)/streams-stream.idl \
	    $(INTERFACES)/files-directory.idl \
	    $(INTERFACES)/common-tree.idl \
	    $(INTERFACES)/files-writable_file.idl \
	    $(INTERFACES)/files-transactional_writable_file.idl \
	    $(INTERFACES)/common-transactional.idl \
	    $(INTERFACES)/files-file.idl \
	    $(INTERFACES)/streams-readable.idl \
	    $(INTERFACES)/streams-releasable_reader.idl \
	    $(INTERFACES)/streams-releasable_writer.idl \
	    $(INTERFACES)/streams-seekable_reader.idl \
	    $(INTERFACES)/streams-reader.idl \
	    $(INTERFACES)/streams-writer.idl \
	    $(INTERFACES)/common-resource.idl \
	    $(INTERFACES)/streams-seekable.idl \
	    $(INTERFACES)/processes-process.idl \
	    $(INTERFACES)/processes-process_control.idl \
	    $(INTERFACES)/processes-segment.idl \
	    $(INTERFACES)/processes-server.idl \
	    $(INTERFACES)/caps-cap.idl \
	    $(INTERFACES)/processes-heap.idl \
	    $(INTERFACES)/storage-region.idl \
	    $(INTERFACES)/storage-region_access.idl \
	    $(INTERFACES)/storage-bank.idl \
	    $(INTERFACES)/storage-dependent.idl \
	    $(INTERFACES)/screens-surface.idl \
	    $(INTERFACES)/caps-bank_constructor.idl \
	    $(INTERFACES)/caps-constructor.idl \
	    $(INTERFACES)/caps-cap_set.idl \
	    $(INTERFACES)/caps-releasable_cap_set.idl \
	    $(INTERFACES)/processes-replaceable.idl \
	    $(INTERFACES)/processes-saveable.idl \
	    $(INTERFACES)/processes-replacer.idl \
	    $(INTERFACES)/processes-replacement.idl \
	    ../replaceable_test/held_call.idl
	$(IDLE) --target user --command-client -I $(INTERFACES) \
	    -o build/generated \
	    ../../drivers/file/rose-core-file.idl \
	    ../../drivers/directory/rose-core-directory.idl \
	    $(INTERFACES)/rose-core.idl
	$(IDLE) --target user --command-client -I $(INTERFACES) \
	    -o build/generated \
	    ../user/rose-core-user.idl ../auth/rose-core-auth.idl \
	    $(INTERFACES)/rose-core.idl
	gprbuild $(GPRBUILD_CARGS)
