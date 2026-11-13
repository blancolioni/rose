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
	    $(INTERFACES)/user.idl \
	    $(INTERFACES)/replaceable.idl \
	    $(INTERFACES)/saveable.idl \
	    $(INTERFACES)/releasable_cap_set.idl \
	    $(INTERFACES)/cap_set.idl \
	    $(INTERFACES)/constructor.idl \
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
	    $(INTERFACES)/reader.idl \
	    $(INTERFACES)/writer.idl \
	    $(INTERFACES)/resource.idl \
	    $(INTERFACES)/bank.idl \
	    $(INTERFACES)/dependent.idl \
	    $(INTERFACES)/region.idl \
	    $(INTERFACES)/seekable.idl
	gprbuild $(GPRBUILD_CARGS)
