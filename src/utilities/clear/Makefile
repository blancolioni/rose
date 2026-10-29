ARCH=i686
IDLE ?= idle
INTERFACES=../../../interfaces

all: clear
	cp ./build/bin/clear-driver ../../../build/$(ARCH)/drivers/clear

# The spec of Rose.Core.Clear, its main and its metadata, and the package
# Rose.Core, are generated from IDL (ADR 0002).

clear:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target kernel -I $(INTERFACES) -o build/generated \
	    rose-core-clear.idl $(INTERFACES)/rose-core.idl
	gprbuild
