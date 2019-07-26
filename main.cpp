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
  // term_run(tty);

  // {
  //   native_string file_name = "COPYPA.TXT";
  //   file* f;
  //   if (NO_ERROR == open_file(file_name, &f)) {
  //     // need free... lets use the static buff

  //     uint8* buff = (uint8*) kmalloc(f->length * sizeof(char));

  //     debug_write("[COPYPASTA] File length:");
  //     debug_write(f->length);
  //     debug_write(f->current_cluster);
  //     debug_write(f->current_section_length);
  //     debug_write(f->current_section_pos);
  //     debug_write(f->current_section_start);
      
  //     error_code err;
  //     debug_write("Starting to read the file...");
  //     if (ERROR(err = read_file(f, buff, f->length))) {
  //       term_write(tty, "ERROR WHILE READING:\n");
  //       term_write(tty, err);
  //       fatal_error("ERR");
  //     }
  //     debug_write("Done writing... printing soon");

  //     term_write(tty, '---\n\r');
  //     term_write(tty, CAST(native_string, buff + f->length - 100));
  //     term_write(tty, "!");
  //     term_write(tty, "\n\r----");
  //   } else {
  //     term_write(tty, "\r\n Failed to read the file.\r\n");
  //   }
  // }

  // __asm__ __volatile__("int $6" : : : "memory");

  {
    native_string file_name = "GSI.EXE";

    file* prog;
    if (NO_ERROR == open_file(file_name, &prog)) {
      term_write(tty, "\r\n Starting program ");
      term_write(tty, file_name);
      term_writeline(tty);

      // TODO:
      // The program thread needs to be aware of what its doing
      uint32 len = prog->length;
      uint8* code = (uint8*)GAMBIT_START;

      debug_write("File length: ");
      debug_write(len);

      error_code err;
      if (ERROR(err = read_file(prog, code, len))) {
        fatal_error("ERR");
      }

      term_write(cout, "File loaded. Starting program at: ");
      term_write(cout, code);
      
      thread::sleep(1000);

      for (int i = 0; i < 5; ++i) {
        term_writeline(cout);
      }

      program_thread* task = new program_thread(CAST(libc_startup_fn, code));
      task->start();

    } else {
      term_write(tty, "\r\n Failed to open the program.\r\n");
    }
  }

  // Never exit, but never do anything either
  for(;;) thread::yield();
  
  return 0;
}

//-----------------------------------------------------------------------------
