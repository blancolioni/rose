ROOT=../../..
include $(ROOT)/mk/common.mk

all: touch
	cp ./build/bin/touch-driver ../../../build/$(ARCH)/drivers/touch

# The spec of Rose.Core.Touch, its main and its metadata, the package
# Rose.Core, and Transactional_Writable_File with the interfaces it
# uses, are generated from IDL (ADR 0002) for the user target.

touch:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) \
	    rose-core-touch.idl $(INTERFACES)/rose-core.idl \
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
