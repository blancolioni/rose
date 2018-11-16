.global _start
.text
_start:
    call    main
    movl    $0x04000040, exit_params
    movl    $0x01, exit_params + 4
    push   $exit_params
    int     $0x30
    0: nop
    jmp 0b
    
.data

exit_params: 
    .skip 8
    