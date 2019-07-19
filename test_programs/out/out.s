.code32
start_of_program:
    popl %eax
    popl %eax
#debug_a:
#   jmp debug_a
    movl %eax, %ebx
pop_loop:
    movb $'O', %al
    outb %al, $0xE9

    movl $5, %eax

    pushl %eax
    call *%ebx
    popl %eax


    jmp pop_loop
    out_msg:
        .ascii "This is loaded from the disk"
        .byte 0
end_of_program:
