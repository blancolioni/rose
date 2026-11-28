ROOT=../../..
include $(ROOT)/mk/common.mk

all: trellis
	cp ./build/bin/trellis ../../../build/$(ARCH)/drivers/trellis

# trellis, the display server (ADR 0022, #583), is built with the userland
# run-time system, and serves Display and its surfaces through the
# user-mode IDL target. It is a command, Rose.Core.Trellis: the spec of the
# command, its main and its metadata are generated here. Rose.Core and the
# interfaces it uses come from the userland library
# (projects/user_library.gpr, #1067), and Rose.Drawing from
# src/library/drawing.

trellis:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) rose-core-trellis.idl
	gprbuild $(GPRBUILD_CARGS)
