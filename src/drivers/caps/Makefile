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
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
	    $(INTERFACES)/constructor.idl $(INTERFACES)/cap_set.idl \
	    $(INTERFACES)/resource.idl $(INTERFACES)/releasable_cap_set.idl \
	    $(INTERFACES)/restorable.idl
	gprbuild $(GPRBUILD_CARGS)
