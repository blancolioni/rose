ROOT=../../..
include $(ROOT)/mk/common.mk

all: hold_pages
	cp ./build/bin/hold_pages-driver ../../../build/$(ARCH)/drivers/hold_pages

# The spec of Rose.Core.Hold_Pages, its main and its metadata are
# generated from IDL (ADR 0002) for the user target. Rose.Core and the
# interfaces hold_pages uses come from the userland library
# (projects/user_library.gpr, #1067).

hold_pages:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) rose-core-hold_pages.idl
	gprbuild $(GPRBUILD_CARGS)
