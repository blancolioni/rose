ROOT=../../..
include $(ROOT)/mk/common.mk

all: hold_derived
	cp ./build/bin/hold_derived-driver ../../../build/$(ARCH)/drivers/hold_derived

# The spec of Rose.Core.Hold_Derived, its main and its metadata, the
# package Rose.Core, and the interfaces it uses, are generated from IDL
# (ADR 0002) for the user target.

hold_derived:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target user -I $(INTERFACES) -o build/generated 	    rose-core-hold_derived.idl $(INTERFACES)/rose-core.idl 	    $(INTERFACES)/commands.idl 	    $(INTERFACES)/launch.idl 	    $(INTERFACES)/process.idl 	    $(INTERFACES)/segment.idl 	    $(INTERFACES)/server.idl 	    $(INTERFACES)/heap.idl 	    $(INTERFACES)/cap.idl 	    $(INTERFACES)/restorable.idl 	    $(INTERFACES)/writer.idl 	    $(INTERFACES)/stream.idl
	gprbuild $(GPRBUILD_CARGS)
