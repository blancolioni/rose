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
	    $(INTERFACES)/replaceable.idl \
	    $(INTERFACES)/saveable.idl \
	    $(INTERFACES)/releasable_cap_set.idl \
	    $(INTERFACES)/cap_set.idl \
	    $(INTERFACES)/constructor.idl \
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
