ROOT=../../..
include $(ROOT)/mk/common.mk

all: hold_transient
	cp ./build/bin/hold_transient-driver ../../../build/$(ARCH)/drivers/hold_transient

# The spec of Rose.Core.Hold_Transient, its main and its metadata are
# generated from IDL (ADR 0002) for the user target. Rose.Core and the
# interfaces hold_transient uses come from the userland library
# (projects/user_library.gpr, #1067).

hold_transient:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) rose-core-hold_transient.idl
	gprbuild $(GPRBUILD_CARGS)
