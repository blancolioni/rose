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
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
	    rose-core-petal.idl $(INTERFACES)/rose-core.idl \
	    $(INTERFACES)/commands.idl \
	    $(INTERFACES)/launch.idl \
	    $(INTERFACES)/process.idl \
	    $(INTERFACES)/segment.idl \
	    $(INTERFACES)/server.idl \
	    $(INTERFACES)/cap.idl \
	    $(INTERFACES)/heap.idl \
	    $(INTERFACES)/region.idl \
	    $(INTERFACES)/terminal.idl \
	    $(INTERFACES)/stream.idl \
	    $(INTERFACES)/reader.idl \
	    $(INTERFACES)/writer.idl \
	    $(INTERFACES)/clock.idl \
	    $(INTERFACES)/rights.idl \
	    $(INTERFACES)/user.idl \
	    $(INTERFACES)/named.idl \
	    $(INTERFACES)/writable_directory.idl \
	    $(INTERFACES)/directory.idl \
	    $(INTERFACES)/writable_file.idl \
	    $(INTERFACES)/transactional_writable_file.idl \
	    $(INTERFACES)/transactional.idl \
	    $(INTERFACES)/file.idl \
	    $(INTERFACES)/releasable_reader.idl \
	    $(INTERFACES)/releasable_writer.idl \
	    $(INTERFACES)/seekable_reader.idl \
	    $(INTERFACES)/resource.idl \
	    $(INTERFACES)/seekable.idl
	gprbuild petal.gpr $(GPRBUILD_CARGS)

# The decisions of sudo are plain Ada, so they are tested on the host.

.PHONY: test

test:
	mkdir -p build/test
	cd build/test && gnatmake -gnata -I../../src -I../../src/launch \
	    ../../test/privileges_test.adb && ./privileges_test
