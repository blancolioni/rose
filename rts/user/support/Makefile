ROOT=../../..
include $(ROOT)/mk/common.mk

OS=rose
TARGET=$(ARCH)-$(OS)
BUILD=../build/$(TARGET)
ADAINCLUDE=$(BUILD)/adainclude
ADALIB=$(BUILD)/adalib
OBJ=$(BUILD)/obj
SOURCES=memchr.c memcmp.c memcpy.c memmove.c memset.c strlen.c rose-time.c rose-unwind.c
OBJECTS=$(SOURCES:%.c=%.o) raise-gcc.o $(UNWIND_OBJECTS) $(DIVISION_OBJECTS)

# Exception propagation (#170): GNAT's personality routine, from GCC's
# sources (gcc/README), and the unwinder from the installed GCC.

GCC_INCLUDE=$(shell $(GCC) -m32 -print-file-name=include)
LIBGCC_EH=$(shell $(GCC) -m32 -print-file-name=libgcc_eh.a)
UNWIND_OBJECTS=unwind-dw2.o unwind-dw2-fde-dip.o

# 64-bit division (#497): the compiler calls these for the Duration and
# Time arithmetic of Ada.Calendar, and the installed GCC's 32-bit libgcc has
# them, with nothing else that they need.

LIBGCC=$(shell $(GCC) -m32 -print-libgcc-file-name)
DIVISION_OBJECTS=_divdi3.o _moddi3.o _divmoddi4.o _udivdi3.o _umoddi3.o \
    _udivmoddi4.o
RAISE_OPTS=-isystem $(GCC_INCLUDE) -Igcc -DIN_RTS -DSTANDALONE -DGXX_EH_INTEROP=0

all: librose-util

clean:
	rm -f librose-util.a $(OBJECTS)

librose-util: $(OBJECTS)
	ar cr librose-util.a $(OBJECTS)
	ranlib librose-util.a


%.o: %.c
	$(GCC) -c $(GCC_OPTS) $(@:%.o=%.c)

raise-gcc.o: gcc/raise-gcc.c
	$(GCC) -c $(GCC_OPTS) $(RAISE_OPTS) gcc/raise-gcc.c -o raise-gcc.o

$(UNWIND_OBJECTS):
	ar x $(LIBGCC_EH) $@

$(DIVISION_OBJECTS):
	ar x $(LIBGCC) $@
