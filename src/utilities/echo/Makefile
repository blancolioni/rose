ROOT=../../..
include $(ROOT)/mk/common.mk

all: echo
	cp ./build/bin/echo-driver ../../../build/$(ARCH)/drivers/echo

# The spec of Rose.Core.Echo, its main and its metadata are generated from
# IDL (ADR 0002) for the user target (#371): echo is built with the
# userland run-time system. Rose.Core and the interfaces echo uses come
# from the userland library (projects/user_library.gpr, #1067).

echo:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) rose-core-echo.idl
	gprbuild $(GPRBUILD_CARGS)

# Built for the host, to test it without Rose (#1101).
include $(ROOT)/mk/host.mk
