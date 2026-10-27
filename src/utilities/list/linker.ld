OUTPUT_FORMAT(elf32-i386)
ENTRY (_start)

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

    /* The command's metadata (ADR 0002), which the shell's registry
       reads from the executable. It is not loaded, and is kept even if
       unreferenced sections are discarded. */

    .rose_command 0 (INFO) : {
          KEEP (*(.rose_command))
    }
}
