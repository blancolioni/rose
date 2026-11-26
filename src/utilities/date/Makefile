ROOT=../../..
include $(ROOT)/mk/common.mk

all: date
	cp ./build/bin/date-driver ../../../build/$(ARCH)/drivers/date

# The spec of Rose.Core.Date, its main and its metadata are generated from
# IDL (ADR 0002) for the user target. Rose.Core and the interfaces date
# uses come from the userland library (projects/user_library.gpr, #1067).

date:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) rose-core-date.idl
	gprbuild $(GPRBUILD_CARGS)

# Built for the host, to test it without Rose (#1101).
include $(ROOT)/mk/host.mk
