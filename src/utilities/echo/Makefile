ARCH=i686
IDLE ?= idle
INTERFACES=../../../interfaces

all: echo
	cp ./build/bin/echo-driver ../../../build/$(ARCH)/drivers/echo

# The spec of Rose.Core.Echo, its main and its metadata, and the package
# Rose.Core, are generated from IDL (ADR 0002), for the user target
# (#371): echo is built with the userland run-time system.

echo:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
	    rose-core-echo.idl $(INTERFACES)/rose-core.idl
	gprbuild
