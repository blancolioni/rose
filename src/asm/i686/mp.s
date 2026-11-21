.set END_OF_INT, 0x20
.set INT_CTL_MASTER, 0x20
.set INT_CTL_SLAVE, 0xA0
.set OCW3_READ_IRR, 0x0A
.set OCW3_READ_ISR, 0x0B

.set DIVIDE_VECTOR,              0x00
.set DEBUG_VECTOR,               0x01
.set NMI_VECTOR,                 0x02
.set BREAKPOINT_VECTOR,          0x03
.set OVERFLOW_VECTOR,            0x04
.set BOUND_VECTOR,               0x05
.set INVALID_OPCODE_VECTOR,      0x06
.set NO_DEVICE_VECTOR,           0x07
.set DOUBLE_FAULT_VECTOR,        0x08
.set SEGMENT_OVERRUN_VECTOR,     0x09
.set INVALID_TSS_VECTOR,         0x0A
.set SEGMENT_NOT_PRESENT_VECTOR, 0x0B
.set STACK_FAULT_VECTOR,         0x0C
.set PROTECTION_FAULT_VECTOR,    0x0D
.set PAGE_FAULT_VECTOR,          0x0E
.set RESERVED_15_VECTOR,         0x0F

.global divide_exception, debug_exception, nmi_exception, breakpoint_exception
.global overflow_exception, bound_exception, invalid_opcode_exception, no_device_exception
.global double_fault_exception, segment_overrun_exception, invalid_tss_exception, segment_not_present_exception
.global stack_fault_exception, protection_fault_exception, protection_fault_exception, page_fault_exception
.global reserved_15_exception

# The invocation record, Rose.Invocation.Invocation_Record (#892). The
# kernel copies in the first two words and what the control word says was
# sent, and copies out the six header words and what was sent. The bit
# positions are fixed by the representation clause of Control_Word.
.set INVOKE_HEADER_WORDS, 6        # Control .. Identifier and Unused
.set INVOKE_DATA, 24               # Data, Max_Parameter_Words words
.set INVOKE_CAPS, 56               # Caps, Max_Capabilities words
.set INVOKE_BUFFER, 72             # Buffer_Address, then Buffer_Length

.set WRITABLE_BUFFER_BIT, 1 << 0   # Flags (Writable_Buffer)
.set SEND_BUFFER_BIT, 1 << 1       # Flags (Send_Buffer)
.set SEND_WORDS_BIT, 1 << 9        # Flags (Send_Words)
.set SEND_CAPS_BIT, 1 << 11        # Flags (Send_Caps)
.set LAST_SENT_WORD_SHIFT, 17      # Last_Sent_Word, 3 bits
.set LAST_SENT_CAP_SHIFT, 23       # Last_Sent_Cap, 2 bits

.set CURR_PROC_PTR, 20     # offset from stack pointer to process table entry address after trap

.set Process_Stack_Top, 15 * 4   # must match size of record in Rose.Kernel.Arch
.set Process_Directory_Page, 15 * 4   # offset of Directory_Page field in process structure

.set Process_GS, 0
.set Process_FS, Process_GS + 2
.set Process_ES, Process_FS + 2
.set Process_DS, Process_ES + 2
.set Process_EDI, Process_DS + 2
.set Process_ESI, Process_EDI + 4
.set Process_EBP, Process_ESI + 4
.set Process_XSP, Process_EBP + 4
.set Process_EBX, Process_XSP + 4
.set Process_EDX, Process_EBX + 4
.set Process_ECX, Process_EDX + 4
.set Process_EAX, Process_ECX + 4
.set Process_EIP, Process_EAX + 4
.set Process_CS, Process_EIP + 4
.set Process_PSW, Process_CS + 4
.set Process_ESP, Process_PSW + 4
.set Process_SS, Process_ESP + 4

    .global kernel_stack_top
    .global system_call
    .global continue
    .global idle
    .extern invoke_reply
    
.data
kernel_stack_top:
    .long 0
.text

# Copy the data words, caps and buffer that the control word in %edx says
# were sent, from the record at %ebx to the record at %eax. Uses %ecx,
# %esi and %edi; the direction flag must be clear.
.macro copy_sent_items
    test    $SEND_WORDS_BIT, %edx
    jz      .Lno_words\@
    mov     %edx, %ecx
    shr     $LAST_SENT_WORD_SHIFT, %ecx
    and     $7, %ecx
    inc     %ecx
    lea     INVOKE_DATA(%ebx), %esi
    lea     INVOKE_DATA(%eax), %edi
    rep movsl
.Lno_words\@:
    test    $SEND_CAPS_BIT, %edx
    jz      .Lno_caps\@
    mov     %edx, %ecx
    shr     $LAST_SENT_CAP_SHIFT, %ecx
    and     $3, %ecx
    inc     %ecx
    lea     INVOKE_CAPS(%ebx), %esi
    lea     INVOKE_CAPS(%eax), %edi
    rep movsl
.Lno_caps\@:
    test    $(SEND_BUFFER_BIT | WRITABLE_BUFFER_BIT), %edx
    jz      .Lno_buffer\@
    mov     INVOKE_BUFFER(%ebx), %ecx
    mov     %ecx, INVOKE_BUFFER(%eax)
    mov     INVOKE_BUFFER + 4(%ebx), %ecx
    mov     %ecx, INVOKE_BUFFER + 4(%eax)
.Lno_buffer\@:
.endm

# Data segment selectors. The CPU loads only CS and SS on a way into the
# kernel, and sysexit loads only CS and SS on the way out, so the kernel
# loads the others itself (#1031). An iret to user mode replaces a data
# segment register that holds the kernel's selector with null, and on
# real hardware (not QEMU) the next access through it is a #GP.
.set KERNEL_DATA_SELECTOR, 0x10
.set USER_DATA_SELECTOR, 0x23     # GDT entry 4, RPL 3

# Load the kernel's data selector into DS and ES, using %ax. The kernel
# makes no use of FS or GS.
.macro restore_kernel_segments
    mov     $KERNEL_DATA_SELECTOR, %ax
    mov     %ax, %ds
    mov     %ax, %es
.endm

# Load the user data selector into every data segment register before
# sysexit, using %ax, so a process never runs with the kernel's.
.macro load_user_segments
    mov     $USER_DATA_SELECTOR, %ax
    mov     %ax, %ds
    mov     %ax, %es
    mov     %ax, %fs
    mov     %ax, %gs
.endm

.macro save_registers
    pusha
    pushw %ds
    pushw %es
    pushw %fs
    pushw %gs    
.endm

.macro restore_registers
    popw %gs
    popw %fs
    popw %es
    popw %ds
    popa
.endm

.macro restore_kernel_state
    mov kernel_stack_top, %esp
.endm

.macro save_process_context displacement
    cld
    save_registers                       # the interrupted DS and ES, then
    restore_kernel_segments              # the kernel's (%eax is saved)
    movl    $0, interrupted_from_user
    testl   $3, Process_CS(%esp)         # CPL of saved CS: 0 = ring-0 (kernel), 3 = ring-3 (user)
    jz      1f                            # ring-0 interrupt: skip process structure update

    movl    $1, interrupted_from_user    # the exception handler can't see the CPL once the kernel stack is back
    mov     current_process_ptr, %edi    # destination: process Stack_Frame
    mov     %esp, %esi                   # source: kernel stack (registers + CPU frame)
    movl    $15, %ecx                    # 15 dwords = 60 bytes
	rep movsd
1:
	restore_kernel_state
.endm

.macro hwint_master irq, label
.global \label
\label :
    save_process_context 0
    push    $(\irq + 32)
    call    handle_interrupt
    pop     %ecx
    movb    $(END_OF_INT), %al
    outb    $(INT_CTL_MASTER)
    mov     $invocation_record, %esi
    jmp continue
    .endm

hwint_master 0, hwint0

hwint_master 1, hwint1

hwint_master 2, hwint2

hwint_master 3, hwint3

hwint_master 4, hwint4

hwint_master 5, hwint5

hwint_master 6, hwint6

hwint_master 7, hwint7_dispatch

.macro hwint_slave irq, label
.global \label
\label :
    save_process_context 0
    push    $(\irq + 32)
    call    handle_interrupt
    pop     %ecx
    movb    $(END_OF_INT), %al
    outb    $(INT_CTL_MASTER)
    outb    $(INT_CTL_SLAVE)
    mov     $invocation_record, %esi
    jmp continue
    .endm

hwint_slave 8, hwint8

hwint_slave 9, hwint9

hwint_slave 10, hwint10

hwint_slave 11, hwint11

hwint_slave 12, hwint12

hwint_slave 13, hwint13

hwint_slave 14, hwint14

hwint_slave 15, hwint15_dispatch

# A spurious IRQ 7 or 15: the PIC raised the vector, but the line dropped
# before the CPU acknowledged it, so its in-service bit is clear (#918).
# It goes no further, and the PIC that raised it gets no EOI, which would
# end whatever interrupt it really has in service. For IRQ 15, the master
# did see IRQ 2, so it gets one. A real IRQ 7 or 15 goes to its stub.

.global hwint7
hwint7:
    push    %eax
    movb    $(OCW3_READ_ISR), %al
    outb    $(INT_CTL_MASTER)
    inb     $(INT_CTL_MASTER)
    movb    %al, %ah
    movb    $(OCW3_READ_IRR), %al       # put the read register back
    outb    $(INT_CTL_MASTER)
    testb   $0x80, %ah
    pop     %eax                        # pop leaves the flags alone
    jnz     hwint7_dispatch
    iret

.global hwint15
hwint15:
    push    %eax
    movb    $(OCW3_READ_ISR), %al
    outb    $(INT_CTL_SLAVE)
    inb     $(INT_CTL_SLAVE)
    movb    %al, %ah
    movb    $(OCW3_READ_IRR), %al
    outb    $(INT_CTL_SLAVE)
    testb   $0x80, %ah
    jnz     1f
    movb    $(END_OF_INT), %al
    outb    $(INT_CTL_MASTER)
    pop     %eax
    iret
1:
    pop     %eax
    jmp     hwint15_dispatch

.data
.global exception_code, excep
exception_code:
    .long 0
    
exception_number:
    .long 0

.global interrupted_from_user
interrupted_from_user:
    .long 0

kernel_reentry_count:
    .long -1

.global page_fault_address
page_fault_address:
    .long 0
    
.text

divide_exception:
    pushl $DIVIDE_VECTOR
    jmp cpu_exception
    
debug_exception:
    pushl $DEBUG_VECTOR
    jmp cpu_exception
    
nmi_exception:
    pushl $NMI_VECTOR
    jmp cpu_exception
    
breakpoint_exception:
    pushl $BREAKPOINT_VECTOR
    jmp cpu_exception
    
overflow_exception:
    pushl $OVERFLOW_VECTOR
    jmp cpu_exception
    
bound_exception:
    pushl $BOUND_VECTOR
    jmp cpu_exception
    
invalid_opcode_exception:
    pushl $INVALID_OPCODE_VECTOR
    jmp cpu_exception
    
no_device_exception:
    pushl $NO_DEVICE_VECTOR
    jmp cpu_exception
    
double_fault_exception:
    pushl $DOUBLE_FAULT_VECTOR
    jmp cpu_exception_code
    
segment_overrun_exception:
    pushl $SEGMENT_OVERRUN_VECTOR
    jmp cpu_exception
    
invalid_tss_exception:
    pushl $INVALID_TSS_VECTOR
    jmp cpu_exception_code
    
segment_not_present_exception:
    pushl $SEGMENT_NOT_PRESENT_VECTOR
    jmp cpu_exception_code
    
stack_fault_exception:
    pushl $STACK_FAULT_VECTOR
    jmp cpu_exception_code
    
protection_fault_exception:
    pushl $PROTECTION_FAULT_VECTOR
    jmp cpu_exception_code
    
page_fault_exception:
    pushl %eax
    mov %cr2, %eax
    mov %eax, %ss:page_fault_address     # DS is still the interrupted one
    pop %eax
    pushl $PAGE_FAULT_VECTOR
    jmp cpu_exception_code

reserved_15_exception:
    pushl $RESERVED_15_VECTOR
    jmp cpu_exception

.global cpu_exception

cpu_exception:
    # cpu exception with no error code
    # DS is still the interrupted one, which may be null, so these go
    # through SS, which is always the kernel's here (#1031)
    movl $0, %ss:exception_code
    popl %ss:exception_number
    jmp cpu_exception_common

.global cpu_exception_code

cpu_exception_code:
    # cpu exception with error code
    popl %ss:exception_number
    popl %ss:exception_code
    
cpu_exception_common:
    save_process_context 0    
    mov exception_code, %eax
    push %eax
    mov exception_number, %eax
    push %eax
    call exception_handler
    pop %ecx
    pop %ecx
    mov $invocation_record, %esi
    jmp continue    
        
.text

system_call:
    push    %ebp                            # save %ebp for later
    push    %eax                            # the caller's DS and ES are
    restore_kernel_segments                 # not saved: sysexit loads
    pop     %eax                            # the user selector (#1031)
    mov     current_process_ptr, %ebp       # currently running process
    mov     %ecx, Process_ESP(%ebp)         # %esp is passed to us via %ecx
    mov     %edx, Process_EIP(%ebp)         # %eip is passed to us via %edx
    pushf                                   # get current eflags
    pop     %edx                            
    or      $0x0200, %edx                   # make sure interrupts will be enabled when we return
    mov     %edx, Process_PSW(%ebp)         # save it to the process record
    mov     %esi, Process_ESI(%ebp)
    mov     %edi, Process_EDI(%ebp)
    pop     %esi
    mov     %esi, Process_EBP(%ebp)
    mov     %eax, Process_EAX(%ebp)
    mov     %ebx, Process_EBX(%ebp)

    push    %eax                            # check the caller's record first
    call    is_valid_invocation_record
    add     $4, %esp
    test    %eax, %eax
    jnz     2f
    push    Process_EAX(%ebp)               # not mapped user memory: fault the
    call    fault_invocation_record         # caller, and run the next process
    add     $4, %esp
    mov     $invocation_record, %esi
    jmp     continue
2:
    mov     Process_EAX(%ebp), %ebx            # copy in from the caller's record
    mov     $invocation_record, %eax
    cld                                        # the caller's direction flag is unknown
    mov     (%ebx), %edx                       # the control word, and the cap; the
    mov     %edx, (%eax)                       # kernel never reads the caller's
    mov     4(%ebx), %ecx                      # Reply_Cap, Endpoint or Identifier
    mov     %ecx, 4(%eax)
    copy_sent_items

    push    $invocation_record
    call    invoke_capability
    pop     %esi                               # we need to pop our argument, and conveniently it
                                               # will need to be available in %esi if it's used

continue:
    mov     idle_state, %ebp
    cmp     $0, %ebp
    jnz     idle                               # zero indicates idle state
    mov     current_process_ptr, %ebp          # may or may not be the same process

    cmp     fpu_owner_ptr, %ebp                # does it own the FPU and SSE state?
    je      1f
    mov     %cr0, %eax                         # if not, set TS, so that its first FPU or
    or      $0x08, %eax                        # SSE instruction switches the state
    mov     %eax, %cr0
    jmp     2f
1:  clts
2:
    mov Process_Directory_Page(%ebp), %eax     # copy scheduled process page directory
    mov %cr3, %edx                             # to global page directory register,
    cmp %eax, %edx                             # unless it is there already
    je 3f
    mov %eax, %cr3
3:

    movl    interrupt_resume, %eax             # was this process interrupted?
    cmp     $0, %eax                          
    jnz     rti_continue                       # if so, return via rti

    movl    invoke_reply, %eax
    cmp     $0, %eax                           # are we sending a reply to their invocation?
    jz      no_reply_continue                  # if not, skip invocation record copy
    
    mov     Process_EAX(%ebp), %eax            # process eax had the pointer to user space invocation record
    mov     $invocation_record, %ebx
    mov     %ebx, %esi
    mov     %eax, %edi
    mov     $INVOKE_HEADER_WORDS, %ecx
    cld                                        # an interrupted process may have set it
    rep movsl                                  # the header, which the kernel wrote
    mov     (%ebx), %edx
    copy_sent_items

no_reply_continue:    
    mov     Process_EDI(%ebp), %edi
    mov     Process_ESI(%ebp), %esi
    mov     Process_ESP(%ebp), %ecx            # sysexit returns esp and eip via ecx/edx
    mov     Process_EIP(%ebp), %edx
    mov     Process_EBX(%ebp), %ebx
    pushl   Process_PSW(%ebp)
    lea Process_Stack_Top(%ebp), %eax
    mov %eax, (global_tss + 4)
    load_user_segments                         # sysexit leaves DS and ES alone
    mov     Process_EBP(%ebp), %ebp
    popf
    sysexit
    
rti_continue:
    mov current_process_ptr, %esp
    lea Process_Stack_Top(%esp), %eax
    mov %eax, (global_tss + 4)
    testl   $3, Process_CS(%esp)               # back to user mode?
    jz      1f
    movw    $USER_DATA_SELECTOR, Process_DS(%esp)  # iret would null the kernel's
    movw    $USER_DATA_SELECTOR, Process_ES(%esp)  # selector, so a record saved
    movw    $USER_DATA_SELECTOR, Process_FS(%esp)  # before #1031 gets the user's
    movw    $USER_DATA_SELECTOR, Process_GS(%esp)
1:
    restore_registers
    iret

idle:
    sti
    hlt
    cli
    jmp idle
    
    
# page for system calls

.align 0x1000
.global system_call_entry
system_call_entry:
    push %eax
    mov 0x08(%esp), %eax
    push %ecx
    push %edx
    mov %esp, %ecx
    mov $0xffff0010, %edx
    sysenter
    
.align 4
0:  pop %edx
    pop %ecx
    pop %eax
    ret
    
.align 0x1000
