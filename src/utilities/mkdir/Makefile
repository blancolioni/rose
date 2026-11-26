ROOT=../../..
include $(ROOT)/mk/common.mk

all: mkdir
	cp ./build/bin/mkdir-driver ../../../build/$(ARCH)/drivers/mkdir

# The spec of Rose.Core.Make_Directory, its main and its metadata are
# generated from IDL (ADR 0002) for the user target. Rose.Core and the
# interfaces mkdir uses come from the userland library
# (projects/user_library.gpr, #1067).

mkdir:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) rose-core-make_directory.idl
	gprbuild $(GPRBUILD_CARGS)

# Built for the host, to test it without Rose (#1101).
include $(ROOT)/mk/host.mk
