ROOT=../../..
include $(ROOT)/mk/common.mk

all: supervisor
	cp ./build/bin/supervisor-driver ../../../build/$(ARCH)/drivers/supervisor

# The spec of Rose.Core.Supervisor, its main and its metadata are
# generated from IDL (ADR 0002) for the user target: supervisor is built
# with the userland run-time system (#526). Rose.Core and the interfaces
# supervisor uses come from the userland library
# (projects/user_library.gpr, #1067).

supervisor:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) rose-core-supervisor.idl
	gprbuild $(GPRBUILD_CARGS)
