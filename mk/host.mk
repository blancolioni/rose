# Building a command for the host's own Ada run-time, to test it without
# booting Rose (#1101): "make host" in the command's directory, whose
# Makefile includes this after its own rules. The command is the one
# rose-core-*.idl beside the Makefile. Its spec, main and metadata are
# generated with idle --target host and the host backings
# (interfaces/host-backings.txt, idle#133), and its executable is
# build/host/bin/NAME, where NAME is the directory's name.

HOST_IDL := $(wildcard rose-core-*.idl)
HOST_MAIN := $(HOST_IDL:.idl=_main.adb)
HOST_NAME := $(notdir $(CURDIR))

.PHONY: host
host:
	$(MAKE) -C $(ROOT)/src/library/host
	mkdir -p build/host/generated build/host/obj build/host/bin
	$(IDLE) --target host --host-backings $(INTERFACES)/host-backings.txt \
	    -I $(INTERFACES) -o build/host/generated \
	    $(HOST_IDL) $(INTERFACES)/rose-core.idl
	gprbuild -q -p -P $(ROOT)/projects/host_command.gpr \
	    -XHOST_COMMAND_DIR=$(CURDIR) -XHOST_MAIN=$(HOST_MAIN)
	cp build/host/bin/$(HOST_MAIN:.adb=) build/host/bin/$(HOST_NAME)
