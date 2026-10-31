ROOT=../../..
include $(ROOT)/mk/common.mk

all: cat
	cp ./build/bin/cat-driver ../../../build/$(ARCH)/drivers/cat

# The spec of Rose.Core.Cat, its main and its metadata, the package
# Rose.Core, and the File interface with its readers and their parents,
# are generated from IDL (ADR 0002) for the user target: cat is built
# with the userland run-time system.

cat:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
	    rose-core-cat.idl $(INTERFACES)/rose-core.idl \
	    $(INTERFACES)/file.idl $(INTERFACES)/file_reader.idl \
	    $(INTERFACES)/seekable_file_reader.idl $(INTERFACES)/resource.idl \
	    $(INTERFACES)/stream_reader.idl $(INTERFACES)/seekable.idl
	gprbuild
