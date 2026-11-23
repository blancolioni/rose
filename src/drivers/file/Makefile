ROOT=../../..
include $(ROOT)/mk/common.mk

all: file
	cp ./build/bin/file ../../../build/$(ARCH)/drivers/file

# file is built with the userland run-time system (#490), and serves the
# file through the user-mode IDL target (#754). It is a command,
# Rose.Core.File, which a directory launches, and a replaceable server
# (#867): the spec of the command, its main and its metadata, the package
# Rose.Core, and the interfaces it uses, with their parents, are generated
# here. Rose.Replacement needs the packages of Replaceable,
# Releasable_Reader, Releasable_Cap_Set and Constructor, with their
# parents.

file:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
	    rose-core-file.idl \
	    $(INTERFACES)/rose-core.idl \
	    $(INTERFACES)/processes-replaceable.idl \
	    $(INTERFACES)/processes-saveable.idl \
	    $(INTERFACES)/caps-releasable_cap_set.idl \
	    $(INTERFACES)/caps-cap_set.idl \
	    $(INTERFACES)/caps-bank_constructor.idl \
	    $(INTERFACES)/storage-bank.idl \
	    $(INTERFACES)/storage-dependent.idl \
	    $(INTERFACES)/storage-region.idl \
	    $(INTERFACES)/caps-constructor.idl \
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
