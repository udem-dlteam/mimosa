void main() {
    
    int i = 0;

    for(;;) {
        if(i % 2 == 0) {
            __asm__ __volatile__("mov $'0', %%al\n outb %%al, $0xe9" : : : "memory" );
        } else {
            __asm__ __volatile__("mov $'1', %%al\n outb %%al, $0xe9" : : : "memory" );
        }
        i += 1 % 2;
    }

}