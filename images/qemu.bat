"C:\Program Files\qemu\qemu-system-i386.exe" -drive file=rose-boot-floppy.img,index=0,if=floppy,format=raw -m 8 -serial file:../logs/qemu-port-1.txt -drive file=rose-hd0.img,index=0,if=ide,format=raw -drive file=rose.iso,if=ide,index=1,media=cdrom
@rem "C:\Program Files\qemu\qemu-system-i386.exe" -drive file=rose-boot-floppy.img,index=0,if=floppy,format=raw -m 8 -serial file:../logs/qemu-port-1.txt -drive file=rose-hd0.img,index=0,index=0,format=raw -cdrom rose.iso
@rem below is the interrupt trace call
@rem "C:\Program Files\qemu\qemu-system-i386.exe" -drive file=rose-boot-floppy.img,index=0,if=floppy,format=raw -m 8 -serial file:../logs/qemu-port-1.txt -drive file=rose-hd0.img,index=0,index=0,format=raw -d int > trace.txt 2>&1
