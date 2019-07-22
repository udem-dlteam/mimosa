.code32
start_of_program:
#    popl %eax
#    popl %eax
##debug_a:
##   jmp debug_a
#    movl %eax, %ebx
#
#    movl $out_msg, %eax
#
#    pushl %eax
#    call *%ebx
#    popl %eax

pop_loop:
    movb $'O', %al
    outb %al, $0xE9
    jmp pop_loop
    out_msg:
        .ascii "This is a string loaded from a compiled program :D"
        .byte 0
end_of_program:
