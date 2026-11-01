ROOT=../../..
include $(ROOT)/mk/common.mk

all: petal
	cp ./build/bin/petal-driver ../../../build/$(ARCH)/drivers/petal

# Petal is a command (#457): the spec of Rose.Core.Petal, its main and its
# metadata, and the package Rose.Core, are generated from IDL (ADR 0002).

petal:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target kernel -I $(INTERFACES) -o build/generated \
	    rose-core-petal.idl $(INTERFACES)/rose-core.idl
	gprbuild petal.gpr $(GPRBUILD_CARGS)

# The decisions of sudo are plain Ada, so they are tested on the host.

.PHONY: test

test:
	mkdir -p build/test
	cd build/test && gnatmake -gnata -I../../src -I../../src/launch \
	    ../../test/privileges_test.adb && ./privileges_test
