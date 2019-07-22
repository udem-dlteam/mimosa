
typedef struct user_func_table {
  void (*print_int)(int i);
  void (*print_str)(char* str);
  void (*print_int_ptr)(int* i);
} user_func_table;

void main() {
    
    user_func_table* table = (user_func_table*) 0x1F000;

    int i = 0;

    for(;;) {
        if(i % 2 == 0) {
            __asm__ __volatile__("mov $'0', %%al\n outb %%al, $0xe9" : : : "memory" );
        } else {
            __asm__ __volatile__("mov $'1', %%al\n outb %%al, $0xe9" : : : "memory" );
        }
        i += 1;
        table->print_str("The big brown dog jumps over the lazy fox! :D\r\n");
        table->print_int_ptr(&i);
    }

}