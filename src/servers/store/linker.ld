OUTPUT_FORMAT(elf32-i386)
STARTUP(./build/obj/b__store-driver.o)
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
        __bss_start = .;
        *(COMMON)
        *(.bss)
        _end = .;
    }

    .note : {
          *(.note)
    }

    .debug : {
          *(.debug)
    }
}
