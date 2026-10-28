#!/bin/sh
# Boot Rose from hd0, with no install media (#322).
qemu-system-i386 -m 32 -serial file:qemu-port-1.txt -drive file=rose-hd0.img,index=0,if=ide,format=raw
