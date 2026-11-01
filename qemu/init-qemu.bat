if not exist images\rose-hd0.img "C:\Program Files\qemu\qemu-img.exe"  create -f raw images\rose-hd0.img 6G
"C:\Program Files\qemu\qemu-system-i386.exe" -m 32 -boot d -serial file:logs\qemu-port-1.txt -drive file=images\rose-hd0.img,index=0,if=ide,format=raw -drive file=Z:\git\rose\images\rose.iso,if=ide,index=1,media=cdrom
