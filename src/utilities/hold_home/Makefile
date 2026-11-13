ROOT=../../..
include $(ROOT)/mk/common.mk

all: hold_home
	cp ./build/bin/hold_home-driver ../../../build/$(ARCH)/drivers/hold_home

# The spec of Rose.Core.Hold_Home, its main and its metadata, the package
# Rose.Core, and the interfaces it uses, are generated from IDL (ADR 0002)
# for the user target, and the client of the user command, which it
# launches (#831, idle#103).

hold_home:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
	    rose-core-hold_home.idl $(INTERFACES)/rose-core.idl \
	    $(INTERFACES)/commands.idl \
	    $(INTERFACES)/constructor.idl \
	    $(INTERFACES)/launch.idl \
	    $(INTERFACES)/process.idl \
	    $(INTERFACES)/bank.idl \
	    $(INTERFACES)/dependent.idl \
	    $(INTERFACES)/resource.idl \
	    $(INTERFACES)/region.idl \
	    $(INTERFACES)/segment.idl \
	    $(INTERFACES)/server.idl \
	    $(INTERFACES)/heap.idl \
	    $(INTERFACES)/cap.idl \
	    $(INTERFACES)/restorable.idl \
	    $(INTERFACES)/user.idl \
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
	    $(INTERFACES)/seekable.idl \
	    $(INTERFACES)/reader.idl \
	    $(INTERFACES)/writer.idl \
	    $(INTERFACES)/stream.idl
	$(IDLE) --target user --command-client -I $(INTERFACES) \
	    -o build/generated \
	    ../user/rose-core-user.idl $(INTERFACES)/rose-core.idl
	gprbuild $(GPRBUILD_CARGS)
