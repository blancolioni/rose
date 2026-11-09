OUTPUT_FORMAT(elf32-i386)
ENTRY (_start)

SECTIONS
{
    /* The VGA text page is identity-mapped, at 0xB8000 (the device-page
       cap in vga-console.driver), so the image starts above the first
       megabyte. At 0 it grew into the page once it kept its names
       (#490), and the screen's cells overwrote its variables. */

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

ASSERT (ADDR (.text) >= 0x000C0000,
        "vga-console: the image overlaps the VGA text page at 0xB8000")
