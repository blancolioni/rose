ROOT=../../..
include $(ROOT)/mk/common.mk

all: login
	cp ./build/bin/login-driver ../../../build/$(ARCH)/drivers/login

# The spec of Rose.Core.Login, its main and its metadata are generated
# from IDL (ADR 0002) for the user target: login is built with the
# userland run-time system (#457). Rose.Core and the interfaces login uses
# come from the userland library (projects/user_library.gpr, #1067).

login:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) rose-core-login.idl
	gprbuild $(GPRBUILD_CARGS)
