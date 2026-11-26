ROOT=../../..
include $(ROOT)/mk/common.mk

all: hold_derived
	cp ./build/bin/hold_derived-driver ../../../build/$(ARCH)/drivers/hold_derived

# The spec of Rose.Core.Hold_Derived, its main and its metadata are
# generated from IDL (ADR 0002) for the user target. Rose.Core and the
# interfaces hold_derived uses come from the userland library
# (projects/user_library.gpr, #1067).

hold_derived:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) rose-core-hold_derived.idl
	gprbuild $(GPRBUILD_CARGS)
