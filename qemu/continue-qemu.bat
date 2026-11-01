rem Boot Rose from hd0, with no install media (#322).
"C:\Program Files\qemu\qemu-system-i386.exe" -m 32 -serial file:logs\qemu-port-1.txt -drive file=images\rose-hd0.img,index=0,if=ide,format=raw
