# Settings that the Makefiles share.  A Makefile sets ROOT to the path from
# its own directory to the top of the tree, and includes this:
#
#   ROOT=../../..
#   include $(ROOT)/mk/common.mk
#
# This only sets variables, so it can go before a Makefile's first rule.

ARCH=i686
GCC=gcc

# What the servers' C is compiled with, and the library's util.c, which
# they link.
GCC_OPTS=-std=gnu99 -ffreestanding -g -finline-limit=3000 -fno-strict-aliasing -Wall -Winline -Wno-format -Wno-char-subscripts -Werror -m32 -nostdinc -nostdlib
UTIL=$(ROOT)/src/library/util.c

# The IDL compiler, and the interfaces it compiles.  IDLE can be set in the
# environment, to use one that isn't on the path.
IDLE ?= idle
INTERFACES=$(ROOT)/interfaces

# The run-time system that the servers are built with.  The programs that
# are built with the userland run-time system, src/rts, name it in their
# project files.
KERNEL_RTS=$(ROOT)/rts/build/$(ARCH)
