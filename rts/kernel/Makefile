ROOT=../..
include $(ROOT)/mk/common.mk

# The run-time system for the kernel and the servers, KERNEL_RTS in mk/common.mk.
# The drivers and the utilities have the other one, in rts/user.

BUILD=./build/$(ARCH)
ADAINCLUDE=$(BUILD)/adainclude
ADALIB=$(BUILD)/adalib
OBJ=$(BUILD)/obj

all: clean config copy rts

clean:
	rm -rf build
	rm -rf gnat-src

config:
	mkdir -p $(ADAINCLUDE)
	mkdir -p $(ADALIB)
	mkdir -p $(OBJ)

copy:
	sh ./copy-runtime.sh

rts:
	gprbuild -v rts.gpr
