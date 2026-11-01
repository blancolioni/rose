#!/bin/sh
# Boot Rose from hd0, with no install media (#322).  The disk is
# images/rose-hd0.img, and serial output goes to logs/qemu-port-1.txt.
qemu-system-i386 -m 32 -serial file:logs/qemu-port-1.txt -drive file=images/rose-hd0.img,index=0,if=ide,format=raw
