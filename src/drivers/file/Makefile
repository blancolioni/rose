ARCH=i686

all: file
	cp ./build/bin/file ../../../build/$(ARCH)/drivers/file

# file is built with the userland run-time system (#490).

file:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild
