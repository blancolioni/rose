ROOT=../../..
include $(ROOT)/mk/common.mk

all: directory
	cp ./build/bin/directory ../../../build/$(ARCH)/drivers/directory

# directory is built with the userland run-time system (#490), and serves
# Writable_Directory through the user-mode IDL target (#528). The
# interfaces it uses, with their parents, are generated here, and the
# client of the file command, which it launches (#867, idle#103).

directory:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
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
	    $(INTERFACES)/seekable.idl \
	    $(INTERFACES)/launch.idl \
	    $(INTERFACES)/process.idl \
	    $(INTERFACES)/bank.idl $(INTERFACES)/dependent.idl \
	    $(INTERFACES)/segment.idl \
	    $(INTERFACES)/server.idl \
	    $(INTERFACES)/cap.idl \
	    $(INTERFACES)/heap.idl \
	    $(INTERFACES)/region.idl
	$(IDLE) --target user --command-client -I $(INTERFACES) \
	    -o build/generated \
	    ../file/rose-core-file.idl $(INTERFACES)/rose-core.idl
	gprbuild $(GPRBUILD_CARGS)
