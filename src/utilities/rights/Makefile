ROOT=../../..
include $(ROOT)/mk/common.mk

all: rights
	cp ./build/bin/rights-driver ../../../build/$(ARCH)/drivers/rights

# The spec of Rose.Core.Rights, its main and its metadata, the package
# Rose.Core, and Rights with the interfaces it uses, are generated from
# IDL (ADR 0002) for the user target.

rights:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
	    rose-core-rights.idl $(INTERFACES)/rose-core.idl \
	    $(INTERFACES)/users-rights.idl \
	    $(INTERFACES)/users-rights_admin.idl \
	    $(INTERFACES)/time-clock_admin.idl \
	    $(INTERFACES)/time-clock.idl \
	    $(INTERFACES)/processes-restorable.idl \
	    $(INTERFACES)/users-user.idl \
	    $(INTERFACES)/common-named.idl \
	    $(INTERFACES)/streams-writer.idl \
	    $(INTERFACES)/files-writable_directory.idl \
	    $(INTERFACES)/files-directory.idl \
	    $(INTERFACES)/files-writable_file.idl \
	    $(INTERFACES)/files-transactional_writable_file.idl \
	    $(INTERFACES)/common-transactional.idl \
	    $(INTERFACES)/files-file.idl \
	    $(INTERFACES)/streams-releasable_reader.idl \
	    $(INTERFACES)/streams-releasable_writer.idl \
	    $(INTERFACES)/streams-seekable_reader.idl \
	    $(INTERFACES)/streams-reader.idl \
	    $(INTERFACES)/common-resource.idl \
	    $(INTERFACES)/storage-bank.idl \
	    $(INTERFACES)/storage-dependent.idl \
	    $(INTERFACES)/storage-region.idl \
	    $(INTERFACES)/streams-seekable.idl
	gprbuild $(GPRBUILD_CARGS)

# The policy is plain Ada, so it is tested on the host.

.PHONY: test

test:
	mkdir -p build/test
	cd build/test && gnatmake -gnata -I../../src \
	    ../../test/rights_policy_test.adb && ./rights_policy_test
