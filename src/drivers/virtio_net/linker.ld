OUTPUT_FORMAT(elf32-i386)
ENTRY (_start)

SECTIONS
{
    /* The DMA pages are identity-mapped, at 0x10000 to 0x1FFFF (the
       device-pages cap in virtio_net.driver), so the image starts above
       the first megabyte (#291, #295). */

    . = 0x00100000;

    .text : {
        *(.text)
    }

    .rodata : ALIGN (0x1000) {
        *(.rodata)
    }

    /* The unwind tables, for exception propagation (#170). */

    .eh_frame_hdr : {
        __eh_frame_hdr_start = .;
        *(.eh_frame_hdr)
    }

    .eh_frame : {
        KEEP (*(.eh_frame))
    }

    .gcc_except_table : {
        *(.gcc_except_table .gcc_except_table.*)
    }

    .data : ALIGN (0x1000) {
        *(.data)
    }

    .bss : {
        *(COMMON)
        *(.bss)
    }

    .note : {
          *(.note)
    }

    .debug : {
          *(.debug)
    }
}

ASSERT (ADDR (.text) >= 0x00020000,
        "virtio_net: the image overlaps its DMA pages at 0x10000")
