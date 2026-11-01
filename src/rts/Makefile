ROOT=../..
include $(ROOT)/mk/common.mk

# The userland run-time system, USER_RTS in mk/common.mk, for the drivers and the
# utilities.  The kernel and the servers have the other one, in rts.

OS=rose
TARGET=$(ARCH)-$(OS)
BUILD=./build/$(TARGET)
ADAINCLUDE=$(BUILD)/adainclude
ADALIB=$(BUILD)/adalib
OBJ=$(BUILD)/obj

all: clean config copy support init rts

clean:
	rm -rf build
	rm -f start/start.o
	$(MAKE) -C support clean

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
