ROOT=../../..
include $(ROOT)/mk/common.mk

all: virtio_net
	cp ./build/bin/virtio_net ../../../build/$(ARCH)/drivers/virtio_net

# virtio_net is built with the userland run-time system, and serves
# Network_Device through the user-mode IDL target (#295), as ps2 does.

virtio_net:
	mkdir -p build/obj
	mkdir -p build/bin
	gprbuild $(GPRBUILD_CARGS)
