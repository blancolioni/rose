ROOT=../../..
include $(ROOT)/mk/common.mk

all: copy
	cp ./build/bin/copy-driver ../../../build/$(ARCH)/drivers/copy

# The spec of Rose.Core.Copy, its main and its metadata, the package
# Rose.Core, and File and Writable_File with the interfaces they use, are
# generated from IDL (ADR 0002) for the user target.

copy:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
	    rose-core-copy.idl $(INTERFACES)/rose-core.idl \
	    $(INTERFACES)/writable_file.idl \
	    $(INTERFACES)/file.idl \
	    $(INTERFACES)/file_reader.idl \
	    $(INTERFACES)/seekable_file_reader.idl \
	    $(INTERFACES)/stream_reader.idl \
	    $(INTERFACES)/stream_writer.idl \
	    $(INTERFACES)/resource.idl \
	    $(INTERFACES)/seekable.idl
	gprbuild $(GPRBUILD_CARGS)
