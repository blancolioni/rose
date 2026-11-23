ROOT=../../..
include $(ROOT)/mk/common.mk

all: petal
	cp ./build/bin/petal-driver ../../../build/$(ARCH)/drivers/petal

# Petal is a command (#457): the spec of Rose.Core.Petal, its main and its
# metadata, the package Rose.Core, and the interfaces petal uses, are
# generated from IDL (ADR 0002) for the user target (#793).

petal:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) \
	    rose-core-petal.idl $(INTERFACES)/rose-core.idl \
	    $(INTERFACES)/commands-lookup.idl \
	    $(INTERFACES)/processes-launch.idl \
	    $(INTERFACES)/processes-process.idl \
	    $(INTERFACES)/processes-process_control.idl \
	    $(INTERFACES)/storage-bank.idl $(INTERFACES)/storage-dependent.idl \
	    $(INTERFACES)/processes-segment.idl \
	    $(INTERFACES)/processes-server.idl \
	    $(INTERFACES)/caps-cap.idl \
	    $(INTERFACES)/processes-heap.idl \
	    $(INTERFACES)/storage-region.idl \
	    $(INTERFACES)/storage-region_access.idl \
	    $(INTERFACES)/screens-terminal.idl \
	    $(INTERFACES)/streams-stream.idl \
	    $(INTERFACES)/streams-reader.idl \
	    $(INTERFACES)/streams-writer.idl \
	    $(INTERFACES)/time-clock.idl \
	    $(INTERFACES)/users-rights.idl \
	    $(INTERFACES)/users-user.idl \
	    $(INTERFACES)/common-named.idl \
	    $(INTERFACES)/files-writable_directory.idl \
	    $(INTERFACES)/files-fixed_directory.idl \
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
	    $(INTERFACES)/common-resource.idl \
	    $(INTERFACES)/streams-seekable.idl
	gprbuild petal.gpr $(GPRBUILD_CARGS)

# The decisions of sudo are plain Ada, so they are tested on the host.

.PHONY: test

test:
	mkdir -p build/test
	cd build/test && gnatmake -gnata -I../../src -I../../src/launch \
	    ../../test/privileges_test.adb && ./privileges_test
