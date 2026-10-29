ARCH=i686
IDLE ?= idle
INTERFACES=../../../interfaces

all: run_self_tests
	cp ./build/bin/run_self_tests-driver ../../../build/$(ARCH)/drivers/run_self_tests

# The spec of Rose.Core.Run_Self_Tests, its main and its metadata, and the
# package Rose.Core, are generated from IDL (ADR 0002, #460). Like the
# user command, it uses the kernel-target interface clients that the
# tests use.

run_self_tests:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target kernel -I $(INTERFACES) -o build/generated \
	    rose-core-run_self_tests.idl $(INTERFACES)/rose-core.idl
	gprbuild
