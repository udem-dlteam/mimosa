// file: "main.cpp"

// Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 23 Oct 01  initial version (Marc Feeley)

//-----------------------------------------------------------------------------

#include "general.h"
#include "term.h"
#include "thread.h"
#include "chrono.h"
#include "disk.h"
#include "fs.h"
#include "fat32.h"
#include "ps2.h"

//-----------------------------------------------------------------------------

extern "C" void println(uint32 str) {
  // for(;;);

  // thread* current = thread::self();
  // uint32 base = (uint32)current->_code;

  // term_write(cout, "Addressed passed: ");
  // term_write(cout, (void*)str);
  // term_writeline(cout);

  // str += base;

  // term_write(cout, "Addressed calculated: ");
  // term_write(cout, (void*)str);
  // term_writeline(cout);

  // native_string a_str = (char*)str;
  // term_write(cout, a_str);

  // term_writeline(cout);
}

void __attribute__((optimize("O0"))) _user_print_int(int i) {
  term_write(cout, "User print:");
  term_write(cout, i);
  term_writeline(cout);
}

void __attribute__((optimize("O0"))) _user_print_int_ptr(int* i) {
  term_write(cout, "User print int ptr:");
  thread* curr = thread::self();
  //i += curr->code();
  term_write(cout, *i);
  term_writeline(cout);
}

void __attribute__((optimize("O0"))) _user_print_str(char* str) {
  // Correct the pointer address:
  thread* curr = thread::self();
  // str += curr->code();
  term_write(cout, "User print str: \r\n");
  term_write(cout, str);
  term_writeline(cout);
}

int main() {

  user_func_table* table = (user_func_table*) 0x1F000;
  table->print_int = _user_print_int;
  table->print_str = _user_print_str;
  table->print_int_ptr = _user_print_int_ptr;

  term* tty = &new_term(0, 320, 80, 10, &font_mono_6x9, L"tty", true);
  term_run(tty);
  return 0;
}

//-----------------------------------------------------------------------------
