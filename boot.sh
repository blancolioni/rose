#!/bin/sh
qemu-img create -f raw rose-hd0.img 128M
qemu-system-i386 -m 8 -serial file:qemu-port-1.txt -drive file=rose-hd0.img,index=0,if=ide,format=raw -drive file=./images/rose.iso,if=ide,index=1,media=cdrom
