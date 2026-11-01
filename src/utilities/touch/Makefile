ROOT=../../..
include $(ROOT)/mk/common.mk

all: touch
	cp ./build/bin/touch-driver ../../../build/$(ARCH)/drivers/touch

# The spec of Rose.Core.Touch, its main and its metadata, and the package
# Rose.Core, are generated from IDL (ADR 0002).

touch:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target kernel -I $(INTERFACES) -o build/generated \
	    rose-core-touch.idl $(INTERFACES)/rose-core.idl
	gprbuild $(GPRBUILD_CARGS)
