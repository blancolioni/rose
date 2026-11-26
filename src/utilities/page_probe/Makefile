ROOT=../../..
include $(ROOT)/mk/common.mk

all: page_probe
	cp ./build/bin/page_probe-driver ../../../build/$(ARCH)/drivers/page_probe

# The spec of Rose.Core.Page_Probe, its main and its metadata are
# generated from IDL (ADR 0002) for the user target. Rose.Core and the
# interfaces page_probe uses come from the userland library
# (projects/user_library.gpr, #1067).

page_probe:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) rose-core-page_probe.idl
	gprbuild $(GPRBUILD_CARGS)
