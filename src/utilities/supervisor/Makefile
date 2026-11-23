ROOT=../../..
include $(ROOT)/mk/common.mk

all: supervisor
	cp ./build/bin/supervisor-driver ../../../build/$(ARCH)/drivers/supervisor

# The spec of Rose.Core.Supervisor, its main and its metadata, the package
# Rose.Core, and the interfaces that supervisor uses with their parents, are
# generated from IDL (ADR 0002) for the user target: supervisor is built
# with the userland run-time system (#526).

supervisor:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) \
	    rose-core-supervisor.idl $(INTERFACES)/rose-core.idl \
	    $(INTERFACES)/users-auth.idl \
	    $(INTERFACES)/screens-supervisor.idl \
	    $(INTERFACES)/processes-restorable.idl \
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
	gprbuild $(GPRBUILD_CARGS)
