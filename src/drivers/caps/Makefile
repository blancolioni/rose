ROOT=../../..
include $(ROOT)/mk/common.mk

all: caps
	cp ./build/bin/caps ../../../build/$(ARCH)/drivers/caps

# caps is built with the userland run-time system, and serves its
# interfaces through the user-mode IDL target (#825).

caps:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) \
	    $(INTERFACES)/caps-bank_constructor.idl \
	    $(INTERFACES)/storage-bank.idl \
	    $(INTERFACES)/storage-dependent.idl \
	    $(INTERFACES)/storage-region.idl \
	    $(INTERFACES)/streams-reader.idl \
	    $(INTERFACES)/caps-constructor.idl $(INTERFACES)/caps-cap_set.idl \
	    $(INTERFACES)/common-resource.idl $(INTERFACES)/caps-releasable_cap_set.idl \
	    $(INTERFACES)/processes-restorable.idl
	gprbuild $(GPRBUILD_CARGS)
