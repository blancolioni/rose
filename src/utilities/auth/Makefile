ROOT=../../..
include $(ROOT)/mk/common.mk

all: auth
	cp ./build/bin/auth-driver ../../../build/$(ARCH)/drivers/auth

# The spec of Rose.Core.Auth, its main and its metadata, and the package
# Rose.Core, are generated from IDL (ADR 0002).

auth:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target kernel -I $(INTERFACES) -o build/generated \
	    rose-core-auth.idl $(INTERFACES)/rose-core.idl
	gprbuild
