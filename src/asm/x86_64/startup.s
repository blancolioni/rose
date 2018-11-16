        .global boot                            # Make the startup entry point symbol visible to the linker
        .global boot_PML4T
        .global boot_PDPT
        .global boot_PDT
        .global boot_PT

                                        # Setup the Multiboot header (see GRUB docs for details)
.set ALIGN,    1<<0                     # Align loaded modules on page boundaries
.set MEMINFO,  1<<1                     # Provide memory map
.set FLAGS,    ALIGN | MEMINFO          # This is the Multiboot 'flag' field
.set MAGIC,    0x1BADB002               # 'magic number' lets your bootloader find the header
.set CHECKSUM, -(MAGIC + FLAGS)         # Checksum required

        .set KVB32, 0xC0000000          # what our address looks like in 32 bit mode
        .set KVB64, 0xFFFFFFFFC0000000  # the same, in 64 bit mode

        .data
        .align 0x1000
boot_PML4T:
        .skip 0x1000
boot_PDPT:
        .skip 0x1000
boot_PDT:
        .skip 0x1000
boot_PT:
        .skip 0x1000

        .text

header:                                 # Must be in the first 8kb of the kernel file
.align 4, 0x90                          # Pad the location counter (in the current subsection) to a 4-byte (DWORD) storage boundary.
                                        # The way alignment is specified can vary with host system architecture.
.long MAGIC
.long FLAGS
.long CHECKSUM

                                        # Reserve initial kernel stack space
.set STACKSIZE, 0x4000                  # 0x4000 being 16k.
.lcomm stack, STACKSIZE                 # Reserve 16k stack on a doubleword (32bit) boundary
.comm  mbd, 4                           # Declare common symbol mbd, allocate it 4-bytes (DWORD) of uninitialized memory.
.comm  magic, 4                         # Declare common symbol magic, allocate it 4-bytes (DWORD) of uninitialized memory.

        # we start in 32 bit protected mode, so always use KVB32 until we switch to 64 bit mode

        .set boot, _boot - KVB32

_boot:
        movl  $0x027D0440,0x000b8000    # @}

    movl  %eax, magic - KVB32      # EAX indicates to the OS that it was loaded by a Multiboot-compliant boot loader
    movl  %ebx, mbd - KVB32        # EBX must contain the physical address of the Multiboot information structure


        # set up paging
        # first, clear page tables

        movl $(boot_PML4T - KVB32), %edi    # start of page table
        mov %rdi, %cr3       # cr3 points to the level 4 page map directory
        xor %eax, %eax       # fill page tables with zero
        movl $4096, %ecx     # 4096 32-bit words fills the four tables
1:      mov %eax, (%edi)
        addl $4, %esi
        loop 1b

        mov %cr3, %rdi      # PML4T address in edi


        movl  $0x022C022D,0x000b8004    # -,


    cli                                 # Disable interrupts. then intentionally hang the system.
                                        # CLI only affects the interrupt flag for the processor on which it is executed.

hang:
    hlt                                 # Because the HLT instruction halts until an interrupt occurs, the combination of a
                                        # CLI followed by a HLT is used to intentionally hang the computer if the kernel returns.
                                        # HLT is in a loop just in case a single HLT instruction fails to execute for some reason,
                                        # (such as in the case of an NMI).
    jmp   hang
