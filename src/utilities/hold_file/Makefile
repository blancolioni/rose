ROOT=../../..
include $(ROOT)/mk/common.mk

all: hold_file
	cp ./build/bin/hold_file-driver ../../../build/$(ARCH)/drivers/hold_file

# The spec of Rose.Core.Hold_File, its main and its metadata are generated
# from IDL (ADR 0002) for the user target, with the client of the file
# command, which it launches (#867, idle#103). Rose.Core and the
# interfaces hold_file uses come from the userland library
# (projects/user_library.gpr, #1067).

hold_file:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) rose-core-hold_file.idl
	$(USER_IDLE) --command-client ../../drivers/file/rose-core-file.idl
	gprbuild $(GPRBUILD_CARGS)
