ROOT=../../..
include $(ROOT)/mk/common.mk

all: vga_console
	cp ./build/bin/vga-console ../../../build/$(ARCH)/drivers/vga-console

# vga-console is built with the userland run-time system, and serves its
# interfaces through the user-mode IDL target (#367), whose packages come
# from the userland library (projects/user_library.gpr, #1067). It uses
# its own Process interface to keep what is typed in unsaved memory
# (Rose.Unsaved_Memory, #1132).

vga_console:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild $(GPRBUILD_CARGS)
