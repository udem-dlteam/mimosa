
typedef struct user_func_table {
  void (*print_int)(int i);
  void (*print_str)(char* str);
} user_func_table;

void main() {
    
    user_func_table* table = (user_func_table*) 0x1F000;

    int i = 0;

    for(;;) {
        if(i % 2 == 0) {
            table->print_int(i);
            __asm__ __volatile__("mov $'0', %%al\n outb %%al, $0xe9" : : : "memory" );
        } else {
            table->print_int(i);
            __asm__ __volatile__("mov $'1', %%al\n outb %%al, $0xe9" : : : "memory" );
        }
        i += 1;
        i = i % 2;
        table->print_str("The big brown dog jumps over the lazy fox! :D\r\n");
    }

}