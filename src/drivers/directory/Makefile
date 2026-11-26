ROOT=../../..
include $(ROOT)/mk/common.mk

all: directory
	cp ./build/bin/directory ../../../build/$(ARCH)/drivers/directory

# directory is built with the userland run-time system (#490), and serves
# Writable_Directory through the user-mode IDL target (#528). It is a
# command, Rose.Core.Directory, which auth and other directories launch,
# and a replaceable server (#831): the spec of the command, its main and
# its metadata are generated here, and the clients of the file and
# directory commands, which it launches for its entries (#867, idle#103).
# Rose.Core and the interfaces it uses come from the userland library
# (projects/user_library.gpr, #1067).

directory:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) rose-core-directory.idl
	$(USER_IDLE) --command-client \
	    ../file/rose-core-file.idl \
	    rose-core-directory.idl
	gprbuild $(GPRBUILD_CARGS)
