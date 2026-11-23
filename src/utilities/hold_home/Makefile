ROOT=../../..
include $(ROOT)/mk/common.mk

all: hold_home
	cp ./build/bin/hold_home-driver ../../../build/$(ARCH)/drivers/hold_home

# The spec of Rose.Core.Hold_Home, its main and its metadata, the package
# Rose.Core, and the interfaces it uses, are generated from IDL (ADR 0002)
# for the user target, and the clients of the user and directory
# commands, which it launches (#831, idle#103).

hold_home:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
	    rose-core-hold_home.idl $(INTERFACES)/rose-core.idl \
	    $(INTERFACES)/commands-lookup.idl \
	    $(INTERFACES)/caps-bank_constructor.idl \
	    $(INTERFACES)/caps-constructor.idl \
	    $(INTERFACES)/processes-launch.idl \
	    $(INTERFACES)/processes-process.idl \
	    $(INTERFACES)/processes-process_control.idl \
	    $(INTERFACES)/storage-bank.idl \
	    $(INTERFACES)/storage-dependent.idl \
	    $(INTERFACES)/common-resource.idl \
	    $(INTERFACES)/storage-region.idl \
	    $(INTERFACES)/storage-region_access.idl \
	    $(INTERFACES)/processes-segment.idl \
	    $(INTERFACES)/processes-server.idl \
	    $(INTERFACES)/processes-heap.idl \
	    $(INTERFACES)/caps-cap.idl \
	    $(INTERFACES)/processes-restorable.idl \
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
	    $(INTERFACES)/streams-seekable.idl \
	    $(INTERFACES)/streams-reader.idl \
	    $(INTERFACES)/streams-writer.idl \
	    $(INTERFACES)/streams-stream.idl
	$(IDLE) --target user --command-client -I $(INTERFACES) \
	    -o build/generated \
	    ../user/rose-core-user.idl \
	    ../../drivers/directory/rose-core-directory.idl \
	    $(INTERFACES)/rose-core.idl
	$(DROP_RTS_UNITS)
	gprbuild $(GPRBUILD_CARGS)
