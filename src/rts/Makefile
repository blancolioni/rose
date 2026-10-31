ROOT=../..
include $(ROOT)/mk/common.mk

OS=rose
TARGET=$(ARCH)-$(OS)
BUILD=./build/$(TARGET)
ADAINCLUDE=$(BUILD)/adainclude
ADALIB=$(BUILD)/adalib
OBJ=$(BUILD)/obj

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
