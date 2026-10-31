ROOT=../../..
include $(ROOT)/mk/common.mk

all: clock
	cp ./build/bin/clock ../../../build/$(ARCH)/drivers/clock

# clock is built with the userland run-time system, and serves its
# interfaces through the user-mode IDL target (#367).

clock:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
	    $(INTERFACES)/clock.idl $(INTERFACES)/clock_admin.idl \
	    $(INTERFACES)/restorable.idl
	gprbuild

# The calendar arithmetic is plain Ada, so it is tested on the host.

.PHONY: test

test:
	mkdir -p build/test
	cd build/test && gnatmake -gnata -I../../src -I../../../../base \
	    -I../../../../library/capabilities \
	    ../../test/clock_calendar_test.adb && ./clock_calendar_test
