ROOT=../../..
include $(ROOT)/mk/common.mk

all: directory
	cp ./build/bin/directory ../../../build/$(ARCH)/drivers/directory

# directory is built with the userland run-time system (#490), and serves
# Writable_Directory through the user-mode IDL target (#528). It is a
# command, Rose.Core.Directory, which auth and other directories launch,
# and a replaceable server (#831): the spec of the command, its main and
# its metadata, the package Rose.Core, and the interfaces it uses, with
# their parents, are generated here, and the clients of the file and
# directory commands, which it launches for its entries (#867, idle#103).
# Rose.Replacement needs the packages of Replaceable, Releasable_Reader,
# Releasable_Cap_Set and Constructor, with their parents.

directory:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
	    rose-core-directory.idl \
	    $(INTERFACES)/rose-core.idl \
	    $(INTERFACES)/processes-replaceable.idl \
	    $(INTERFACES)/processes-saveable.idl \
	    $(INTERFACES)/caps-releasable_cap_set.idl \
	    $(INTERFACES)/caps-cap_set.idl \
	    $(INTERFACES)/caps-bank_constructor.idl \
	    $(INTERFACES)/caps-constructor.idl \
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
	    $(INTERFACES)/streams-seekable.idl \
	    $(INTERFACES)/processes-launch.idl \
	    $(INTERFACES)/processes-process.idl \
	    $(INTERFACES)/processes-process_control.idl \
	    $(INTERFACES)/storage-bank.idl \
	    $(INTERFACES)/storage-dependent.idl \
	    $(INTERFACES)/processes-segment.idl \
	    $(INTERFACES)/processes-server.idl \
	    $(INTERFACES)/caps-cap.idl \
	    $(INTERFACES)/processes-heap.idl \
	    $(INTERFACES)/storage-region.idl \
	    $(INTERFACES)/storage-region_access.idl
	$(IDLE) --target user --command-client -I $(INTERFACES) \
	    -o build/generated \
	    ../file/rose-core-file.idl rose-core-directory.idl \
	    $(INTERFACES)/rose-core.idl
	gprbuild $(GPRBUILD_CARGS)
