ROOT=../../..
include $(ROOT)/mk/common.mk

all: null_stream
	cp ./build/bin/null_stream ../../../build/$(ARCH)/drivers/null_stream

# null_stream is built with the userland run-time system, and serves the
# null stream through the user-mode IDL target (ADR 0035, section 4). It
# is a command, Rose.Core.Null_Stream, which init launches once, for the
# system directory: the spec of the command, its main and its metadata are
# generated here. Rose.Core and the interfaces it uses come from the
# userland library (projects/user_library.gpr, #1067).

null_stream:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(USER_IDLE) rose-core-null_stream.idl
	gprbuild $(GPRBUILD_CARGS)
