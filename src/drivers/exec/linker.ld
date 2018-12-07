OUTPUT_FORMAT(elf32-i386)
STARTUP(./build/obj/b__launch-driver.o)
ENTRY (main)

SECTIONS
{
    .text : {
        *(.text)
    }

    .rodata : ALIGN (0x1000) {
        *(.rodata)
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
