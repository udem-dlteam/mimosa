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
#include "rtlib.h"

int main() {

  term* tty = &new_term(0, 320, 80, 10, &font_mono_6x9, L"tty", true);

  // {
  //   native_string file_name = "GSI.EXE";

  //   file* prog;
  //   if (NO_ERROR == open_file(file_name, &prog)) {
  //     term_write(tty, "\r\n Starting program ");
  //     term_write(tty, file_name);
  //     term_writeline(tty);

  //     // TODO:
  //     // The program thread needs to be aware of what its doing
  //     uint32 len = prog->length;
  //     uint8* code = (uint8*)GAMBIT_START;

  //     debug_write("File length: ");
  //     debug_write(len);

  //     error_code err;
  //     if (ERROR(err = read_file(prog, code, len))) {
  //       fatal_error("ERR");
  //     }

  //     term_write(cout, "File loaded. Starting program at: ");
  //     term_write(cout, code);
      
  //     thread::sleep(1000);

  //     for (int i = 0; i < 5; ++i) {
  //       term_writeline(cout);
  //     }

  //     program_thread* task = new program_thread(CAST(libc_startup_fn, code));
  //     task->start();

  //   } else {
  //     term_write(tty, "\r\n Failed to open the program.\r\n");
  //   }
  // }

  
  __surround_with_debug_t("Create file", {
    error_code err;

    if(ERROR(err = create_file(""))) {
      term_write(tty, "Error while creating an empty file");
    } else {
      term_write(tty, "Success while creating an empty file");
    }
  });

  // Never exit, but never do anything either
  for(;;) thread::yield();
  
  return 0;
}

//-----------------------------------------------------------------------------
