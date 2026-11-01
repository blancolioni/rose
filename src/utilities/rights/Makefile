ROOT=../../..
include $(ROOT)/mk/common.mk

all: rights
	cp ./build/bin/rights-driver ../../../build/$(ARCH)/drivers/rights

# The spec of Rose.Core.Rights, its main and its metadata, and the package
# Rose.Core, are generated from IDL (ADR 0002).

rights:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target kernel -I $(INTERFACES) -o build/generated \
	    rose-core-rights.idl $(INTERFACES)/rose-core.idl
	gprbuild $(GPRBUILD_CARGS)

# The policy is plain Ada, so it is tested on the host.

.PHONY: test

test:
	mkdir -p build/test
	cd build/test && gnatmake -gnata -I../../src \
	    ../../test/rights_policy_test.adb && ./rights_policy_test
