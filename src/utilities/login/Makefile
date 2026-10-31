ROOT=../../..
include $(ROOT)/mk/common.mk

all: login
	cp ./build/bin/login-driver ../../../build/$(ARCH)/drivers/login

# The spec of Rose.Core.Login, its main and its metadata, the package
# Rose.Core, and the interfaces that login uses with their parents, are
# generated from IDL (ADR 0002) for the user target: login is built with
# the userland run-time system (#457).

login:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
	    rose-core-login.idl $(INTERFACES)/rose-core.idl \
	    $(INTERFACES)/auth.idl \
	    $(INTERFACES)/user.idl \
	    $(INTERFACES)/named.idl \
	    $(INTERFACES)/terminal.idl \
	    $(INTERFACES)/stream.idl \
	    $(INTERFACES)/stream_reader.idl \
	    $(INTERFACES)/stream_writer.idl \
	    $(INTERFACES)/timer.idl \
	    $(INTERFACES)/clock.idl \
	    $(INTERFACES)/cap.idl \
	    $(INTERFACES)/timeout.idl \
	    $(INTERFACES)/commands.idl \
	    $(INTERFACES)/launch.idl \
	    $(INTERFACES)/writable_directory.idl \
	    $(INTERFACES)/directory.idl \
	    $(INTERFACES)/writable_file.idl \
	    $(INTERFACES)/file.idl \
	    $(INTERFACES)/file_reader.idl \
	    $(INTERFACES)/seekable_file_reader.idl \
	    $(INTERFACES)/resource.idl \
	    $(INTERFACES)/seekable.idl \
	    $(INTERFACES)/process.idl \
	    $(INTERFACES)/segment.idl \
	    $(INTERFACES)/server.idl \
	    $(INTERFACES)/heap.idl \
	    $(INTERFACES)/region.idl
	gprbuild
