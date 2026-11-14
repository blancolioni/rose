ROOT=../../..
include $(ROOT)/mk/common.mk

all: auth
	cp ./build/bin/auth-driver ../../../build/$(ARCH)/drivers/auth

# The spec of Rose.Core.Auth, its main and its metadata, the package
# Rose.Core, and Auth with the interfaces it uses, are generated from IDL
# (ADR 0002) for the user target, and the clients of the user and
# directory commands, which New_User launches (#831).

auth:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
	    rose-core-auth.idl $(INTERFACES)/rose-core.idl \
	    $(INTERFACES)/auth.idl \
	    $(INTERFACES)/auth_admin.idl \
	    $(INTERFACES)/user.idl \
	    $(INTERFACES)/named.idl \
	    $(INTERFACES)/commands.idl \
	    $(INTERFACES)/constructor.idl \
	    $(INTERFACES)/launch.idl \
	    $(INTERFACES)/process.idl \
	    $(INTERFACES)/bank.idl $(INTERFACES)/dependent.idl \
	    $(INTERFACES)/segment.idl \
	    $(INTERFACES)/server.idl \
	    $(INTERFACES)/cap.idl \
	    $(INTERFACES)/heap.idl \
	    $(INTERFACES)/region.idl \
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
	    $(INTERFACES)/seekable.idl
	$(IDLE) --target user --command-client -I $(INTERFACES) \
	    -o build/generated \
	    ../user/rose-core-user.idl \
	    ../../drivers/directory/rose-core-directory.idl \
	    $(INTERFACES)/rose-core.idl
	gprbuild $(GPRBUILD_CARGS)
