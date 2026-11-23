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
	    $(INTERFACES)/users-auth.idl \
	    $(INTERFACES)/users-user.idl \
	    $(INTERFACES)/common-named.idl \
	    $(INTERFACES)/screens-terminal.idl \
	    $(INTERFACES)/streams-stream.idl \
	    $(INTERFACES)/streams-reader.idl \
	    $(INTERFACES)/streams-writer.idl \
	    $(INTERFACES)/time-timer.idl \
	    $(INTERFACES)/time-clock.idl \
	    $(INTERFACES)/users-rights.idl \
	    $(INTERFACES)/caps-cap.idl \
	    $(INTERFACES)/time-timeout.idl \
	    $(INTERFACES)/commands-lookup.idl \
	    $(INTERFACES)/processes-launch.idl \
	    $(INTERFACES)/files-writable_directory.idl \
	    $(INTERFACES)/files-fixed_directory.idl \
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
	    $(INTERFACES)/common-resource.idl \
	    $(INTERFACES)/streams-seekable.idl \
	    $(INTERFACES)/processes-process.idl \
	    $(INTERFACES)/processes-process_control.idl \
	    $(INTERFACES)/storage-bank.idl $(INTERFACES)/storage-dependent.idl \
	    $(INTERFACES)/processes-segment.idl \
	    $(INTERFACES)/processes-server.idl \
	    $(INTERFACES)/processes-heap.idl \
	    $(INTERFACES)/storage-region.idl \
	    $(INTERFACES)/storage-region_access.idl
	$(DROP_RTS_UNITS)
	gprbuild $(GPRBUILD_CARGS)
