ROOT=../../..
include $(ROOT)/mk/common.mk

all: user
	cp ./build/bin/user-driver ../../../build/$(ARCH)/drivers/user

# The spec of Rose.Core.User, its main and its metadata, the package
# Rose.Core, and User with the interfaces it uses, are generated from IDL
# (ADR 0002) for the user target. It is a replaceable server (#831):
# Rose.Replacement needs the packages of Replaceable, Releasable_Reader,
# Releasable_Cap_Set and Constructor, with their parents.

user:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
	    rose-core-user.idl $(INTERFACES)/rose-core.idl \
	    $(INTERFACES)/users-user.idl \
	    $(INTERFACES)/processes-replaceable.idl \
	    $(INTERFACES)/processes-saveable.idl \
	    $(INTERFACES)/caps-releasable_cap_set.idl \
	    $(INTERFACES)/caps-cap_set.idl \
	    $(INTERFACES)/caps-constructor.idl \
	    $(INTERFACES)/common-named.idl \
	    $(INTERFACES)/files-writable_directory.idl \
	    $(INTERFACES)/files-directory.idl \
	    $(INTERFACES)/files-writable_file.idl \
	    $(INTERFACES)/files-transactional_writable_file.idl \
	    $(INTERFACES)/common-transactional.idl \
	    $(INTERFACES)/files-file.idl \
	    $(INTERFACES)/streams-releasable_reader.idl \
	    $(INTERFACES)/streams-releasable_writer.idl \
	    $(INTERFACES)/streams-seekable_reader.idl \
	    $(INTERFACES)/streams-reader.idl \
	    $(INTERFACES)/streams-writer.idl \
	    $(INTERFACES)/common-resource.idl \
	    $(INTERFACES)/storage-bank.idl \
	    $(INTERFACES)/storage-dependent.idl \
	    $(INTERFACES)/storage-region.idl \
	    $(INTERFACES)/streams-seekable.idl
	gprbuild $(GPRBUILD_CARGS)
