ARCH=i686
IDLE ?= idle
INTERFACES=../../../interfaces

all: run_self_tests
	cp ./build/bin/run_self_tests-driver ../../../build/$(ARCH)/drivers/run_self_tests

# The spec of Rose.Core.Run_Self_Tests, its main and its metadata, and the
# package Rose.Core, and the interfaces that the tests use with their
# parents, are generated from IDL (ADR 0002, #460) for the user target
# (#528).

run_self_tests:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
	    rose-core-run_self_tests.idl $(INTERFACES)/rose-core.idl \
	    $(INTERFACES)/commands.idl \
	    $(INTERFACES)/clock.idl \
	    $(INTERFACES)/rights.idl \
	    $(INTERFACES)/rights_admin.idl \
	    $(INTERFACES)/launch.idl \
	    $(INTERFACES)/user.idl \
	    $(INTERFACES)/named.idl \
	    $(INTERFACES)/writable_directory.idl \
	    $(INTERFACES)/directory.idl \
	    $(INTERFACES)/writable_file.idl \
	    $(INTERFACES)/file.idl \
	    $(INTERFACES)/file_reader.idl \
	    $(INTERFACES)/seekable_file_reader.idl \
	    $(INTERFACES)/stream_reader.idl \
	    $(INTERFACES)/stream_writer.idl \
	    $(INTERFACES)/resource.idl \
	    $(INTERFACES)/seekable.idl \
	    $(INTERFACES)/process.idl \
	    $(INTERFACES)/segment.idl \
	    $(INTERFACES)/server.idl \
	    $(INTERFACES)/cap.idl \
	    $(INTERFACES)/heap.idl \
	    $(INTERFACES)/region.idl
	gprbuild
