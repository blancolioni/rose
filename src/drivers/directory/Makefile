ARCH=i686
IDLE ?= idle
INTERFACES=../../../interfaces

all: directory
	cp ./build/bin/directory ../../../build/$(ARCH)/drivers/directory

# directory is built with the userland run-time system (#490), and serves
# Writable_Directory through the user-mode IDL target (#528). The
# interfaces it uses, with their parents, are generated here.

directory:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
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
	    $(INTERFACES)/launch.idl \
	    $(INTERFACES)/process.idl \
	    $(INTERFACES)/segment.idl \
	    $(INTERFACES)/server.idl \
	    $(INTERFACES)/cap.idl \
	    $(INTERFACES)/heap.idl \
	    $(INTERFACES)/region.idl
	gprbuild
