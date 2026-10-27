"C:\Program Files\qemu\qemu-system-i386.exe" -m 32 -boot d -serial file:../logs/qemu-port-1.txt -drive file=rose-hd0.img,index=0,if=ide,format=raw -drive file=rose.iso,if=ide,index=1,media=cdrom
