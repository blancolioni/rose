ROOT=../../..
include $(ROOT)/mk/common.mk

all: user
	cp ./build/bin/user-driver ../../../build/$(ARCH)/drivers/user

# The spec of Rose.Core.User, its main and its metadata, and the package
# Rose.Core, are generated from IDL (ADR 0002).

user:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target kernel -I $(INTERFACES) -o build/generated \
	    rose-core-user.idl $(INTERFACES)/rose-core.idl
	gprbuild $(GPRBUILD_CARGS)
