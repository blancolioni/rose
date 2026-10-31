ROOT=../../..
include $(ROOT)/mk/common.mk

all: date
	cp ./build/bin/date-driver ../../../build/$(ARCH)/drivers/date

# The spec of Rose.Core.Date, its main and its metadata, and the package
# Rose.Core, and the Clock interface, are generated from IDL (ADR 0002)
# for the user target, as Rose.Clock uses its client (#554).

date:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
	    rose-core-date.idl $(INTERFACES)/rose-core.idl \
	    $(INTERFACES)/clock.idl
	gprbuild
