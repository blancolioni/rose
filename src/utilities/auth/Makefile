ROOT=../../..
include $(ROOT)/mk/common.mk

all: auth
	cp ./build/bin/auth-driver ../../../build/$(ARCH)/drivers/auth

# The spec of Rose.Core.Auth, its main and its metadata, the package
# Rose.Core, and Auth with the interfaces it uses, are generated from IDL
# (ADR 0002) for the user target, with what Rose.Replacement needs, as auth
# is a replaceable server (#831), and the clients of the user and
# directory commands, which New_User launches (#831).

auth:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
	    rose-core-auth.idl $(INTERFACES)/rose-core.idl \
	    $(INTERFACES)/users-auth.idl \
	    $(INTERFACES)/processes-replaceable.idl \
	    $(INTERFACES)/processes-saveable.idl \
	    $(INTERFACES)/caps-releasable_cap_set.idl \
	    $(INTERFACES)/caps-cap_set.idl \
	    $(INTERFACES)/users-auth_admin.idl \
	    $(INTERFACES)/users-user.idl \
	    $(INTERFACES)/common-named.idl \
	    $(INTERFACES)/commands-lookup.idl \
	    $(INTERFACES)/caps-bank_constructor.idl \
	    $(INTERFACES)/caps-constructor.idl \
	    $(INTERFACES)/processes-launch.idl \
	    $(INTERFACES)/processes-process.idl \
	    $(INTERFACES)/processes-process_control.idl \
	    $(INTERFACES)/storage-bank.idl $(INTERFACES)/storage-dependent.idl \
	    $(INTERFACES)/processes-segment.idl \
	    $(INTERFACES)/processes-server.idl \
	    $(INTERFACES)/caps-cap.idl \
	    $(INTERFACES)/processes-heap.idl \
	    $(INTERFACES)/storage-region.idl \
	    $(INTERFACES)/storage-region_access.idl \
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
	    $(INTERFACES)/streams-seekable.idl
	$(IDLE) --target user --command-client -I $(INTERFACES) \
	    -o build/generated \
	    ../user/rose-core-user.idl \
	    ../../drivers/directory/rose-core-directory.idl \
	    $(INTERFACES)/rose-core.idl
	gprbuild $(GPRBUILD_CARGS)
