ROOT=../../..
include $(ROOT)/mk/common.mk

all: swap_probe
	cp ./build/bin/swap_probe-driver ../../../build/$(ARCH)/drivers/swap_probe

# The spec of Rose.Core.Swap_Probe, its main and its metadata, the
# package Rose.Core, and the interfaces it uses, are generated from IDL
# (ADR 0002) for the user target.

swap_probe:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
	    rose-core-swap_probe.idl $(INTERFACES)/rose-core.idl \
	    $(INTERFACES)/named.idl \
	    $(INTERFACES)/commands.idl \
	    $(INTERFACES)/launch.idl \
	    $(INTERFACES)/process.idl \
	    $(INTERFACES)/bank.idl \
	    $(INTERFACES)/dependent.idl \
	    $(INTERFACES)/resource.idl \
	    $(INTERFACES)/region.idl \
	    $(INTERFACES)/reader.idl \
	    $(INTERFACES)/segment.idl \
	    $(INTERFACES)/server.idl \
	    $(INTERFACES)/heap.idl \
	    $(INTERFACES)/cap.idl \
	    $(INTERFACES)/restorable.idl \
	    $(INTERFACES)/writer.idl \
	    $(INTERFACES)/stream.idl
	gprbuild $(GPRBUILD_CARGS)
