# Settings that the Makefiles share.  A Makefile sets ROOT to the path from
# its own directory to the top of the tree, and includes this:
#
#   ROOT=../../..
#   include $(ROOT)/mk/common.mk
#
# This only sets variables, so it can go before a Makefile's first rule.

# A Makefile that includes this runs its steps in order, even when make -j is
# passed down to it, because they depend on each other through the files they
# make and not through prerequisites.  The top-level Makefile, which does have
# prerequisites, sets PARALLEL.
ifndef PARALLEL
.NOTPARALLEL:
endif

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

# The compiler records its own command line in the debug information, and
# gprbuild puts its process id in the names of the temporary files on it, so
# a program built twice from the same sources comes out different, and
# nothing can tell that it hasn't changed.  This leaves the command line out.
# It goes last, after gprbuild's other arguments.
GPRBUILD_CARGS=-cargs -gno-record-gcc-switches

# The run-time system that the servers are built with, and the userland
# one, which the other programs name in their project files.
KERNEL_RTS=$(ROOT)/rts/build/$(ARCH)
USER_RTS=$(ROOT)/src/rts/build/$(ARCH)-rose
