#!/bin/sh
# Install Rose from images/rose.iso on the disk images/rose-hd0.img, which is
# made if it isn't there.  Serial output goes to logs/qemu-port-1.txt.
# The guest has a virtio network card on QEMU's user-mode network, which
# gives it 10.0.2.15 by DHCP, a gateway at 10.0.2.2 and DNS at 10.0.2.3
# (#293).
[ -f images/rose-hd0.img ] || qemu-img create -f raw images/rose-hd0.img 6G
qemu-system-i386 -m 32 -boot d -serial file:logs/qemu-port-1.txt \
    -netdev user,id=net0 -device virtio-net-pci,netdev=net0 \
    -drive file=images/rose-hd0.img,index=0,if=ide,format=raw -drive file=./images/rose.iso,if=ide,index=1,media=cdrom
