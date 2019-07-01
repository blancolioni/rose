.set END_OF_INT, 0x20
.set INT_CTL_MASTER, 0x20
.set INT_CTL_SLAVE, 0xA0

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

.set INVOKE_WORDS, 28      # size of invocation record

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

.macro restore_kernel_segments
    
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
    save_registers
    restore_kernel_state
#    push $(continue)
.endm

.macro hwint_master irq
.global hwint\irq
hwint\irq :
    save_process_context 0
    push    $(\irq + 32)
    call    handle_interrupt
    pop     %ecx
    movb    $(END_OF_INT), %al
    outb    $(INT_CTL_MASTER)
    mov     $invocation_record, %esi
    jmp continue
    .endm

hwint_master 0

hwint_master 1

hwint_master 2

hwint_master 3

hwint_master 4

hwint_master 5

hwint_master 6

hwint_master 7

.macro hwint_slave irq
.global hwint\irq
hwint\irq :
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

hwint_slave 8

hwint_slave 9

hwint_slave 10

hwint_slave 11

hwint_slave 12

hwint_slave 13

hwint_slave 14

hwint_slave 15

.data
.global exception_code, excep
exception_code:
    .long 0
    
exception_number:
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
    mov %eax, page_fault_address
    pop %eax
    pushl $PAGE_FAULT_VECTOR
    jmp cpu_exception_code

reserved_15_exception:
    pushl $RESERVED_15_VECTOR
    jmp cpu_exception

.global cpu_exception

cpu_exception:
    # cpu exception with no error code
    movl $0, exception_code
    popl exception_number
    jmp cpu_exception_common
    
.global cpu_exception_code

cpu_exception_code:
    # cpu exception with error code
    popl exception_number
    popl exception_code
    
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
    
    movl %eax, %esi
    movl $invocation_record, %ebp
    movl $INVOKE_WORDS,%ecx
1:	movl (%esi), %eax
    movl %eax, (%ebp)
    addl $4, %esi
    addl $4, %ebp
    loop 1b
   
    push    $invocation_record
    call    invoke_capability
    pop     %esi                               # we need to pop our argument, and conveniently it
                                               # will need to be available in %esi if it's used

continue:
    mov     idle_state, %ebp
    cmp     $0, %ebp
    jnz     idle                               # zero indicates idle state
    mov     current_process_ptr, %ebp          # may or may not be the same process
    mov Process_Directory_Page(%ebp), %eax     # copy scheduled process page directory
    mov %eax, %cr3                             # to global page directory register

    movl    interrupt_resume, %eax             # was this process interrupted?
    cmp     $0, %eax                          
    jnz     rti_continue                       # if so, return via rti

    movl    invoke_reply, %eax
    cmp     $0, %eax                           # are we sending a reply to their invocation?
    jz      no_reply_continue                  # if not, skip invocation record copy
    
    mov Process_EAX(%ebp), %eax                # process eax had the pointer to user space invocation record
    mov %eax, %ebp
    
    movl $INVOKE_WORDS,%ecx
1:	movl (%esi), %eax
    movl %eax, (%ebp)
    addl $4, %esi
    addl $4, %ebp
    loop 1b
    mov     current_process_ptr, %ebp
    
no_reply_continue:    
    mov     Process_EDI(%ebp), %edi
    mov     Process_ESI(%ebp), %esi
    mov     Process_ESP(%ebp), %ecx            # sysexit returns esp and eip via ecx/edx
    mov     Process_EIP(%ebp), %edx
    mov     Process_EBX(%ebp), %ebx
    pushl   Process_PSW(%ebp)
    lea Process_Stack_Top(%ebp), %eax
    mov %eax, (global_tss + 4)
    mov     Process_EBP(%ebp), %ebp
    popf
    sysexit
    
rti_continue:
    mov current_process_ptr, %esp
    lea Process_Stack_Top(%esp), %eax
    mov %eax, (global_tss + 4)
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
