        .global boot                            # Make the startup entry point symbol visible to the linker
        .global boot_page_directory
        .global boot_page_table
    
        .global kernel_page_directory

.set MULTIBOOT2_HEADER_MAGIC, 0xe85250d6
.set GRUB_MULTIBOOT_ARCHITECTURE_I386, 0
.set MULTIBOOT_HEADER_LENGTH, multiboot_header_end - multiboot_header
.set CHECKSUM, -(MULTIBOOT2_HEADER_MAGIC + GRUB_MULTIBOOT_ARCHITECTURE_I386 + (multiboot_header_end - multiboot_header))

.set PAGE_ENTRY_FLAGS,   0x63           # Initial page entry flag value

        .set KERNEL_VIRTUAL_BASE, 0xF0000000
        .set KERNEL_PAGE_NUMBER, KERNEL_VIRTUAL_BASE >> 22

        .set kernel_page_directory, boot_page_directory - KERNEL_VIRTUAL_BASE
        
        .text
		.align 8
		
multiboot_header:
	.long MULTIBOOT2_HEADER_MAGIC           # multiboot 2 magic
	.long GRUB_MULTIBOOT_ARCHITECTURE_I386  # architecture i386 protected mode
	.long MULTIBOOT_HEADER_LENGTH
	.long CHECKSUM
	
	# end tag
	.short 0
	.short 0
	.long 8

multiboot_header_end:
multiboot_entry:

.data
	.align 0x1000
boot_page_directory:
        .skip 0x1000
boot_page_table:
        .skip 0x1000
        
# Reserve initial kernel stack space
.set STACKSIZE, 0x4000                  # 0x4000 being 16k.
.lcomm stack, STACKSIZE                 # Reserve 16k stack on a doubleword (32bit) boundary
.comm  mbd, 4                           # Declare common symbol mbd, allocate it 4-bytes (DWORD) of uninitialized memory.
.comm  magic, 4                         # Declare common symbol magic, allocate it 4-bytes (DWORD) of uninitialized memory.

.text

boot:
_boot:
    movl  %eax, magic - KERNEL_VIRTUAL_BASE      # EAX indicates to the OS that it was loaded by a Multiboot-compliant boot loader
    movl  %ebx, mbd - KERNEL_VIRTUAL_BASE        # EBX must contain the physical address of the Multiboot information structure

        # set up paging
        # build a table of 1024 identity mapped tables
        movl    $(boot_page_table - KERNEL_VIRTUAL_BASE), %esi
        xorl %edx, %edx
        movl $PAGE_ENTRY_FLAGS, %edx
        movl $1024,%ecx
1:	movl %edx, (%esi)
        addl $0x1000,%edx
        addl $4,%esi
        loop 1b

        # clear page directory
        movl    $(boot_page_directory - KERNEL_VIRTUAL_BASE), %esi
        movl $0, %edx
        movl $1024,%ecx
1:	movl %edx,(%esi)
        addl $4,%esi
        loop 1b

        # point 4M starting at 0x0000_0000 and 0xF000_0000 to 0x0000_0000
        movl	$(boot_page_directory - KERNEL_VIRTUAL_BASE), %esi
        movl	$(boot_page_table - KERNEL_VIRTUAL_BASE), %edx
        orl	$PAGE_ENTRY_FLAGS, %edx
        movl	%edx,(%esi)
        movl	%edx,0xF00(%esi)

        # load boot page directory into master mapping register
        movl    $(boot_page_directory - KERNEL_VIRTUAL_BASE), %edx
        mov	%edx,%cr3

        # turn on paging
        mov     %cr0, %eax
        orl     $0x80000000, %eax       # enable PG bit
        mov     %eax, %cr0

        # update eip
        lea    relocated_start, %edx
        jmp *%edx

relocated_start:        # we are now running paged, in 0xF010_0000

    movl $_ld_start, %eax                # save our start and end locations
    movl %eax, _kmem_base
    movl $_ld_end, %eax
    movl %eax, _kmem_bound

    movl  $(stack + STACKSIZE), %esp    # Set up the stack

        call create_gdt
        lgdt global_gdt_pointer
        
        
        call create_boot_interrupt_table        
        movl $(global_idrt_table), global_irq_pointer + 2        
        lidt global_irq_pointer
    
        xor %eax, %eax
        mov global_gdt_pointer, %ax
        movl global_gdt_pointer + 2, %ebx
        movl global_gdt + 8, %edx
        movl global_gdt + 12, %ecx
        
        ljmp $0x08, $1f
1:      movl $0x10, %eax
	    mov %ax,%ds		# after changing gdt.
	    mov %ax,%es
	    mov %ax,%fs
    	mov %ax,%gs
        
	xorl %eax,%eax
	lldt %ax
    
    mov $0x28, %ax
    ltr %ax
    
        call    initialise_pic        

        # Initialise system call registers
        xor %edx, %edx
        mov $0x08, %eax
        mov $0x174, %ecx
        wrmsr
        mov $(stack + STACKSIZE), %eax
        mov $0x175, %ecx
        wrmsr
        mov $system_call, %eax
        mov $0x176, %ecx
        wrmsr
        
        # set clock interrupt rate ~ 100/s
        # this should go into clock module
        mov $0x36, %al
       outb %al, $0x43
        mov $0x9C, %al
        outb %al, $0x40
        mov $0x2E, %al
        outb %al, $0x40
        
        movl $(stack + STACKSIZE), %esp
        mov %esp, kernel_stack_top
        
        call kernel_main
        jmp continue # schedule our first process
        
