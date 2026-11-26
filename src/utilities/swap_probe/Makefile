ROOT=../../..
include $(ROOT)/mk/common.mk

all: swap_probe
	cp ./build/bin/swap_probe-driver ../../../build/$(ARCH)/drivers/swap_probe

# The spec of Rose.Core.Swap_Probe, its main and its metadata are
# generated from IDL (ADR 0002) for the user target. Rose.Core and the
# interfaces swap_probe uses come from the userland library
# (projects/user_library.gpr, #1067).

swap_probe:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) rose-core-swap_probe.idl
	gprbuild $(GPRBUILD_CARGS)
