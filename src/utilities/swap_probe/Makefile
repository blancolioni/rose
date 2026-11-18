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
	    $(INTERFACES)/common-named.idl \
	    $(INTERFACES)/commands-lookup.idl \
	    $(INTERFACES)/processes-launch.idl \
	    $(INTERFACES)/processes-process.idl \
	    $(INTERFACES)/processes-process_control.idl \
	    $(INTERFACES)/storage-bank.idl \
	    $(INTERFACES)/storage-dependent.idl \
	    $(INTERFACES)/common-resource.idl \
	    $(INTERFACES)/storage-region.idl \
	    $(INTERFACES)/storage-region_access.idl \
	    $(INTERFACES)/streams-reader.idl \
	    $(INTERFACES)/processes-segment.idl \
	    $(INTERFACES)/processes-server.idl \
	    $(INTERFACES)/processes-heap.idl \
	    $(INTERFACES)/caps-cap.idl \
	    $(INTERFACES)/processes-restorable.idl \
	    $(INTERFACES)/streams-writer.idl \
	    $(INTERFACES)/streams-stream.idl
	gprbuild $(GPRBUILD_CARGS)
