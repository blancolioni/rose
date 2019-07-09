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
SOURCES=memcmp.c memcpy.c memmove.c memset.c
OBJECTS=memcmp.o memcpy.o memmove.o memset.o

all: librose-util

clean:
	rm librose-util.a $(OBJECTS)

librose-util: $(OBJECTS)
	ar cr librose-util.a $(OBJECTS)
	ranlib librose-util.a


%.o: %.c
	$(GCC) -c $(GCC_OPTS) $(@:%.o=%.c)
