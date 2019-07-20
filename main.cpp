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
#include "time.h"
#include "disk.h"
#include "fs.h"
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

int main() {
  term* tty = &new_term(0, 320, 80, 10, &font_mono_6x9, L"tty", true);
  term_run(tty);
  // disk* main_dsk = disk_find(0x00);


  // if(NULL == main_dsk) {
  //   term_write(tty, "No disk!");
  // } else {
  //   term_write(tty, "Disk found!");
  // }

  // file** files;
  // error_code code = open_file("TEST.TXT", files);

  // if(code == 0) {
  //   term_write(tty, "Opened text file without error\n\r");
  // } else {
  //   term_write(tty, "Failed to open the file :<(\n\r");
  // }

  // file* test_file = files[0];

  // term_write(tty, "File current cluster (expected 3): ");
  // term_write(tty, test_file->current_cluster);
  // term_writeline(tty);


  // uint8 buff[512 * 8];

  // read_file(test_file, buff, 512);

  // term_write(tty, "File has been read."); term_writeline(tty);

  // term_write(tty, "File contents: \n\r");
  // for(int i = 0; i < 5; ++i) term_writeline(tty);

  // term_write(tty, (native_string) buff);

  // term_write(tty, "Attempting to run a program from the disk..\n\r");

  // code = open_file("OUT.BIN", files);

  // if(code == 0) {
  //   term_write(tty, "Opened code file without error\n\r");
  // } else {
  //   term_write(tty, "Failed to open the file :<(\n\r");
  // }

  // file* code_file = files[0];

  // read_file(code_file, buff, 512);

  // program_thread* test = new program_thread((user_task) buff);
  // test->start();

  return 0;
}

//-----------------------------------------------------------------------------
