ROOT=../../..
include $(ROOT)/mk/common.mk

all: set_date
	cp ./build/bin/set_date-driver ../../../build/$(ARCH)/drivers/set_date

# The spec of Rose.Core.Set_Date, its main and its metadata are generated
# from IDL (ADR 0002) for the user target. Rose.Core and the interfaces
# set_date uses come from the userland library (projects/user_library.gpr,
# #1067).

set_date:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) rose-core-set_date.idl
	gprbuild $(GPRBUILD_CARGS)
