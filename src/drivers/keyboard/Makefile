ROOT=../../..
include $(ROOT)/mk/common.mk

all: keyboard
	cp ./build/bin/keyboard ../../../build/$(ARCH)/drivers/keyboard

# keyboard is built with the userland run-time system, and serves
# Event_Source and its interrupt through the user-mode IDL target (#490).
# They and the interfaces they use are generated in build/generated.

keyboard:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
	    $(INTERFACES)/event_source.idl \
	    $(INTERFACES)/event_listener.idl \
	    $(INTERFACES)/interrupt_handler.idl
	gprbuild $(GPRBUILD_CARGS)
