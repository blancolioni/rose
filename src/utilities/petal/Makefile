ARCH=i686

all: petal
	cp ./build/bin/petal-driver ../../../build/$(ARCH)/drivers/petal

petal:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild petal.gpr
