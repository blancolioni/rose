ARCH=i686

all: user
	cp ./build/bin/user-driver ../../../build/$(ARCH)/drivers/user

user:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild
