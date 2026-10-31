ROOT=../../..
include $(ROOT)/mk/common.mk

all: list
	cp ./build/bin/list-driver ../../../build/$(ARCH)/drivers/list

# The spec of Rose.Core.List, its main and its metadata, and the package
# Rose.Core, are generated from IDL (ADR 0002).

list:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target kernel -I $(INTERFACES) -o build/generated \
	    rose-core-list.idl $(INTERFACES)/rose-core.idl
	gprbuild
