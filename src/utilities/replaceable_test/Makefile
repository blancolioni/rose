ROOT=../../..
include $(ROOT)/mk/common.mk

all: replaceable_test
	cp ./build/bin/replaceable_test-driver ../../../build/$(ARCH)/drivers/replaceable_test

# The spec of Rose.Core.Replaceable_Test, its main and its metadata, the
# package Rose.Core, and the interfaces it uses, are generated from IDL
# (ADR 0002) for the user target. Rose.Replacement (#865) needs the
# packages of Replaceable, Releasable_Reader, Releasable_Cap_Set and
# Constructor, with their parents.

replaceable_test:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
	    rose-core-replaceable_test.idl $(INTERFACES)/rose-core.idl \
	    $(INTERFACES)/named.idl \
	    $(INTERFACES)/replaceable.idl \
	    $(INTERFACES)/releasable_reader.idl \
	    $(INTERFACES)/reader.idl \
	    $(INTERFACES)/resource.idl \
	    $(INTERFACES)/releasable_cap_set.idl \
	    $(INTERFACES)/cap_set.idl \
	    $(INTERFACES)/constructor.idl
	gprbuild $(GPRBUILD_CARGS)
