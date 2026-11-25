ROOT=../../..
include $(ROOT)/mk/common.mk

all: list
	cp ./build/bin/list-driver ../../../build/$(ARCH)/drivers/list

# The spec of Rose.Core.List, its main and its metadata, the package
# Rose.Core, and Directory with
# the interfaces it uses, are generated from IDL (ADR 0002)
# for the user target.

list:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) \
	    rose-core-list.idl $(INTERFACES)/rose-core.idl \
	    $(INTERFACES)/files-directory.idl \
	    $(INTERFACES)/common-tree.idl \
	    $(INTERFACES)/files-file.idl \
	    $(INTERFACES)/streams-readable.idl \
	    $(INTERFACES)/streams-releasable_reader.idl \
	    $(INTERFACES)/streams-seekable_reader.idl \
	    $(INTERFACES)/streams-reader.idl \
	    $(INTERFACES)/common-resource.idl \
	    $(INTERFACES)/streams-seekable.idl
	gprbuild $(GPRBUILD_CARGS)

# Built for the host, to test it without Rose (#1101).
include $(ROOT)/mk/host.mk
