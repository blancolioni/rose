/*  The entry point of a program built with the userland run-time system.

    The kernel starts a process with its stack pointer at the top of the
    stack, with nothing above it, so _start has no return address. It
    says so in its call frame information (.cfi_undefined eip), as
    glibc's _start does, so that the unwinder stops here. Without it, an
    exception that nothing handles makes the unwinder read a return
    address from past the top of the stack, and the program faults
    instead of reaching its last chance handler, which for a command
    fails the launch call. */

extern void main(void);
extern void exit(int);

__asm__ (
    "    .text\n"
    "    .globl _start\n"
    "    .type _start, @function\n"
    "_start:\n"
    "    .cfi_startproc\n"
    "    .cfi_undefined eip\n"
    "    xorl %ebp, %ebp\n"
    "    call main\n"
    "    pushl $0\n"
    "    call exit\n"
    "    hlt\n"
    "    .cfi_endproc\n"
    "    .size _start, . - _start\n"
);
