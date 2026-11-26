ROOT=../../..
include $(ROOT)/mk/common.mk

all: rmdir
	cp ./build/bin/rmdir-driver ../../../build/$(ARCH)/drivers/rmdir

# The spec of Rose.Core.Remove_Directory, its main and its metadata are
# generated from IDL (ADR 0002) for the user target. Rose.Core and the
# interfaces rmdir uses come from the userland library
# (projects/user_library.gpr, #1067).

rmdir:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) rose-core-remove_directory.idl
	gprbuild $(GPRBUILD_CARGS)
