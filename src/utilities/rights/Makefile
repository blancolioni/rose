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
	    $(INTERFACES)/rights.idl \
	    $(INTERFACES)/rights_admin.idl \
	    $(INTERFACES)/clock_admin.idl \
	    $(INTERFACES)/clock.idl \
	    $(INTERFACES)/restorable.idl \
	    $(INTERFACES)/user.idl \
	    $(INTERFACES)/named.idl \
	    $(INTERFACES)/writer.idl \
	    $(INTERFACES)/writable_directory.idl \
	    $(INTERFACES)/directory.idl \
	    $(INTERFACES)/writable_file.idl \
	    $(INTERFACES)/transactional_writable_file.idl \
	    $(INTERFACES)/transactional.idl \
	    $(INTERFACES)/file.idl \
	    $(INTERFACES)/releasable_reader.idl \
	    $(INTERFACES)/releasable_writer.idl \
	    $(INTERFACES)/seekable_reader.idl \
	    $(INTERFACES)/reader.idl \
	    $(INTERFACES)/resource.idl \
	    $(INTERFACES)/seekable.idl
	gprbuild $(GPRBUILD_CARGS)

# The policy is plain Ada, so it is tested on the host.

.PHONY: test

test:
	mkdir -p build/test
	cd build/test && gnatmake -gnata -I../../src \
	    ../../test/rights_policy_test.adb && ./rights_policy_test
