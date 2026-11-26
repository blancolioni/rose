ROOT=../../..
include $(ROOT)/mk/common.mk

all: uptime
	cp ./build/bin/uptime-driver ../../../build/$(ARCH)/drivers/uptime

# The spec of Rose.Core.Uptime, its main and its metadata are generated
# from IDL (ADR 0002) for the user target. Rose.Core and the interfaces
# uptime uses come from the userland library (projects/user_library.gpr,
# #1067).

uptime:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) rose-core-uptime.idl
	gprbuild
