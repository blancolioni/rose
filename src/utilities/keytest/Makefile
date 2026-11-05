ROOT=../../..
include $(ROOT)/mk/common.mk

all: keytest
	cp ./build/bin/keytest-driver ../../../build/$(ARCH)/drivers/keytest

# The spec of Rose.Core.Keytest, its main and its metadata, the package
# Rose.Core, and the console's Terminal interface, are generated from IDL
# (ADR 0002) for the user target: keytest is built with the userland
# run-time system.

keytest:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
	    rose-core-keytest.idl $(INTERFACES)/rose-core.idl \
	    $(INTERFACES)/terminal.idl \
	    $(INTERFACES)/stream.idl \
	    $(INTERFACES)/stream_reader.idl \
	    $(INTERFACES)/stream_writer.idl
	gprbuild $(GPRBUILD_CARGS)
