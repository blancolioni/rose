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
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
	    rose-core-remove_directory.idl $(INTERFACES)/rose-core.idl \
	    $(INTERFACES)/writable_directory.idl \
	    $(INTERFACES)/directory.idl \
	    $(INTERFACES)/writable_file.idl \
	    $(INTERFACES)/file.idl \
	    $(INTERFACES)/file_reader.idl \
	    $(INTERFACES)/seekable_file_reader.idl \
	    $(INTERFACES)/stream_reader.idl \
	    $(INTERFACES)/stream_writer.idl \
	    $(INTERFACES)/resource.idl \
	    $(INTERFACES)/seekable.idl
	gprbuild $(GPRBUILD_CARGS)
