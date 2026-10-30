ARCH=i686

all: directory
	cp ./build/bin/directory ../../../build/$(ARCH)/drivers/directory

# directory is built with the userland run-time system (#490).

directory:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild
