ROOT=../../..
include $(ROOT)/mk/common.mk

all: clear
	cp ./build/bin/clear-driver ../../../build/$(ARCH)/drivers/clear

# The spec of Rose.Core.Clear, its main and its metadata, the package
# Rose.Core, and the console's
# Terminal interface, are generated from IDL (ADR 0002)
# for the user target.

clear:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
	    rose-core-clear.idl $(INTERFACES)/rose-core.idl \
	    $(INTERFACES)/screens-terminal.idl \
	    $(INTERFACES)/streams-stream.idl \
	    $(INTERFACES)/streams-reader.idl \
	    $(INTERFACES)/streams-writer.idl
	gprbuild $(GPRBUILD_CARGS)
