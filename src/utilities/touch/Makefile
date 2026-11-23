ROOT=../../..
include $(ROOT)/mk/common.mk

all: touch
	cp ./build/bin/touch-driver ../../../build/$(ARCH)/drivers/touch

# The spec of Rose.Core.Touch, its main and its metadata, the package
# Rose.Core, and Writable_Directory
# with the interfaces it uses, are generated from IDL (ADR 0002)
# for the user target.

touch:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
	    rose-core-touch.idl $(INTERFACES)/rose-core.idl \
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
	$(DROP_RTS_UNITS)
	gprbuild $(GPRBUILD_CARGS)
