
typedef struct user_func_table {
  void (*print_int)(int i);
  void (*print_str)(char* str);
  void (*print_int_ptr)(int* i);
} user_func_table;

void main() {
    user_func_table* table = (user_func_table*) 0x1F000;
    table->print_str("Coucou\r\n");
}