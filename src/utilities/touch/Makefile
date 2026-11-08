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
	    $(INTERFACES)/writer.idl \
	    $(INTERFACES)/resource.idl \
	    $(INTERFACES)/seekable.idl
	gprbuild $(GPRBUILD_CARGS)
