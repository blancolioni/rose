ROOT=../../..
include $(ROOT)/mk/common.mk

all: replaceable_test
	cp ./build/bin/replaceable_test-driver ../../../build/$(ARCH)/drivers/replaceable_test

# The spec of Rose.Core.Replaceable_Test, its main and its metadata, the
# package Rose.Core, and the interfaces it uses, are generated from IDL
# (ADR 0002) for the user target, with its own test interface, Held_Call
# (#879). Rose.Replacement (#865) needs the packages of Replaceable,
# Releasable_Reader, Releasable_Cap_Set and Constructor, with their
# parents.

replaceable_test:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) \
	    rose-core-replaceable_test.idl held_call.idl \
	    $(INTERFACES)/rose-core.idl \
	    $(INTERFACES)/common-named.idl \
	    $(INTERFACES)/processes-replaceable.idl \
	    $(INTERFACES)/processes-saveable.idl \
	    $(INTERFACES)/streams-writer.idl \
	    $(INTERFACES)/streams-releasable_reader.idl \
	    $(INTERFACES)/streams-reader.idl \
	    $(INTERFACES)/common-resource.idl \
	    $(INTERFACES)/caps-releasable_cap_set.idl \
	    $(INTERFACES)/caps-cap_set.idl \
	    $(INTERFACES)/caps-bank_constructor.idl \
	    $(INTERFACES)/storage-bank.idl \
	    $(INTERFACES)/storage-dependent.idl \
	    $(INTERFACES)/storage-region.idl \
	    $(INTERFACES)/caps-constructor.idl
	gprbuild $(GPRBUILD_CARGS)
