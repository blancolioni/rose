ROOT=../../..
include $(ROOT)/mk/common.mk

all: rmdir
	cp ./build/bin/rmdir-driver ../../../build/$(ARCH)/drivers/rmdir

# The spec of Rose.Core.Remove_Directory, its main and its metadata, the
# package Rose.Core, and Writable_Directory with the interfaces it uses,
# are generated from IDL (ADR 0002) for the user target.

rmdir:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) \
	    rose-core-remove_directory.idl $(INTERFACES)/rose-core.idl \
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
	    $(INTERFACES)/streams-seekable.idl
	gprbuild $(GPRBUILD_CARGS)
