ROOT=../../..
include $(ROOT)/mk/common.mk

all: kilo
	cp ./build/bin/kilo-driver ../../../build/$(ARCH)/drivers/kilo

# The spec of Rose.Core.Editors.Kilo, its main and its metadata are
# generated from IDL (ADR 0002) for the user target: kilo is built with
# the userland run-time system. Rose.Core, Rose.Core.Editors and the
# interfaces kilo uses come from the userland library
# (projects/user_library.gpr, #1067).

kilo:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) rose-core-editors-kilo.idl
	gprbuild $(GPRBUILD_CARGS)
