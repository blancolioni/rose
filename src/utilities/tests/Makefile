ARCH=i686

all: tests
	cp ./build/bin/tests-driver ../../../build/$(ARCH)/drivers/runtests

tests:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild
