ARCH=i686
OS=rose
TARGET=$(ARCH)-$(OS)
BUILD=./build/$(TARGET)
ADAINCLUDE=$(BUILD)/adainclude
ADALIB=$(BUILD)/adalib
OBJ=$(BUILD)/obj
GCC=gcc
DEFINES=
GCC_OPTS=-std=gnu99 -ffreestanding -g -finline-limit=3000 -fno-strict-aliasing -Wall -Winline -Wno-format -Wno-char-subscripts -Werror -m32 -nostdinc -nostdlib

all: clean config copy support init rts

clean:
	rm -rf build

config:
	mkdir -p $(ADAINCLUDE)
	mkdir -p $(ADALIB)
	mkdir -p $(OBJ)

copy:
	sh ./create-sources.sh

rts:
	gprbuild -v rts.gpr
	ar r $(ADALIB)/libgnat.a support/*.o

init:	
	gcc -c -m32 start/start.c -o start/start.o

support: support/librose-util.a

support/librose-util.a:
	(cd support ; make)
