ROOT=../../..
include $(ROOT)/mk/common.mk

all: mkdir
	cp ./build/bin/mkdir-driver ../../../build/$(ARCH)/drivers/mkdir

# The spec of Rose.Core.Make_Directory, its main and its metadata, and the
# package Rose.Core, are generated from IDL (ADR 0002).

mkdir:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target kernel -I $(INTERFACES) -o build/generated \
	    rose-core-make_directory.idl $(INTERFACES)/rose-core.idl
	gprbuild
