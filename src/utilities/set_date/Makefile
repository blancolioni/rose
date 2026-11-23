ROOT=../../..
include $(ROOT)/mk/common.mk

all: set_date
	cp ./build/bin/set_date-driver ../../../build/$(ARCH)/drivers/set_date

# The spec of Rose.Core.Set_Date, its main and its metadata, and the
# package Rose.Core, and the Clock_Admin interface with its parent, are
# generated from IDL (ADR 0002) for the user target, as Rose.Clock uses
# the Clock client (#554).

set_date:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) 	    rose-core-set_date.idl $(INTERFACES)/rose-core.idl 	    $(INTERFACES)/time-clock.idl $(INTERFACES)/time-clock_admin.idl
	gprbuild $(GPRBUILD_CARGS)
