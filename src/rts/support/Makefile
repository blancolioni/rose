ARCH=i686
OS=rose
TARGET=$(ARCH)-$(OS)
BUILD=../build/$(TARGET)
ADAINCLUDE=$(BUILD)/adainclude
ADALIB=$(BUILD)/adalib
OBJ=$(BUILD)/obj
GCC=gcc
DEFINES=
GCC_OPTS=-std=gnu99 -ffreestanding -g -finline-limit=3000 -fno-strict-aliasing -Wall -Winline -Wno-format -Wno-char-subscripts -Werror -m32 -nostdinc -nostdlib
SOURCES=memchr.c memcmp.c memcpy.c memmove.c memset.c strlen.c rose-unwind.c
OBJECTS=$(SOURCES:%.c=%.o) raise-gcc.o $(UNWIND_OBJECTS)

# Exception propagation (#170): GNAT's personality routine, from GCC's
# sources (gcc/README), and the unwinder from the installed GCC.

GCC_INCLUDE=$(shell $(GCC) -m32 -print-file-name=include)
LIBGCC_EH=$(shell $(GCC) -m32 -print-file-name=libgcc_eh.a)
UNWIND_OBJECTS=unwind-dw2.o unwind-dw2-fde-dip.o
RAISE_OPTS=-isystem $(GCC_INCLUDE) -Igcc -DIN_RTS -DSTANDALONE -DGXX_EH_INTEROP=0

all: librose-util

clean:
	rm librose-util.a $(OBJECTS)

librose-util: $(OBJECTS)
	ar cr librose-util.a $(OBJECTS)
	ranlib librose-util.a


%.o: %.c
	$(GCC) -c $(GCC_OPTS) $(@:%.o=%.c)

raise-gcc.o: gcc/raise-gcc.c
	$(GCC) -c $(GCC_OPTS) $(RAISE_OPTS) gcc/raise-gcc.c -o raise-gcc.o

$(UNWIND_OBJECTS):
	ar x $(LIBGCC_EH) $@
