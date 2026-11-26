ROOT=../../..
include $(ROOT)/mk/common.mk

all: file
	cp ./build/bin/file ../../../build/$(ARCH)/drivers/file

# file is built with the userland run-time system (#490), and serves the
# file through the user-mode IDL target (#754). It is a command,
# Rose.Core.File, which a directory launches, and a replaceable server
# (#867): the spec of the command, its main and its metadata are generated
# here. Rose.Core and the interfaces it uses come from the userland
# library (projects/user_library.gpr, #1067).

file:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) rose-core-file.idl
	gprbuild $(GPRBUILD_CARGS)
