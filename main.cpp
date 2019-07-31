// file: "main.cpp"

// Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 23 Oct 01  initial version (Marc Feeley)

//-----------------------------------------------------------------------------

#include "chrono.h"
#include "disk.h"
#include "fat32.h"
#include "fs.h"
#include "general.h"
#include "ps2.h"
#include "rtlib.h"
#include "term.h"
#include "thread.h"

int main() {

  term tty;

  term_init(&tty, 0, 366, 80, 13,
            &font_mono_5x7, &font_mono_5x7,
            L"tty", TRUE);

  // term_run(&tty);

  // {
  //   native_string file_name = "COPYPA.TXT";
  //   file* f;
  //   if (NO_ERROR == open_file(file_name, &f)) {
  //     // need free... lets use the static buff

  file* f;

  //     debug_write("[COPYPASTA] File length:");
  //     debug_write(f->length);
  //     debug_write(f->current_cluster);
  //     debug_write(f->current_section_length);
  //     debug_write(f->current_section_pos);
  //     debug_write(f->current_section_start);
      
  //     error_code err;
  //     debug_write("Starting to read the file...");
  //     if (ERROR(err = read_file(f, buff, f->length))) {
  //       term_write(&tty, "ERROR WHILE READING:\n");
  //       term_write(&tty, err);
  //       fatal_error("ERR");
  //     }
  //     debug_write("Done writing... printing soon");

  //     term_write(&tty, '---\n\r');
  //     term_write(&tty, CAST(native_string, buff + f->length - 100));
  //     term_write(&tty, "!");
  //     term_write(&tty, "\n\r----");
  //   } else {
  //     term_write(&tty, "\r\n Failed to read the file.\r\n");
  //   }
  // }

  __surround_with_debug_t("Actual file write...", {
    if (ERROR(err = write_file(f, buff, 1024))) {
      term_write(cout, "Failed to write the content to the file: err= ");
      term_write(cout, err);
      term_writeline(cout);
    } else {
      term_write(cout, "Success writing!");
      term_writeline(cout);
    }
  });

  // close_file(f);

  file_reset_cursor(f);
  // if (HAS_NO_ERROR(err = open_file("TESTTTT.TXT", &f))) {
  {
    char* b[2];
    b[1] = '\0';

    file* prog;
    if (NO_ERROR == open_file(file_name, &prog)) {
      term_write(&tty, "\r\n Starting program ");
      term_write(&tty, file_name);
      term_writeline(&tty);

    file_set_to_absolute_position(f, 511);
    read_file(f, b, 1);
    b[1] = '\0';
    term_write(cout, "Reading: ");
    term_write(cout, CAST(native_string, b));
    term_writeline(cout);

    file_set_to_absolute_position(f, 513);
    read_file(f, b, 1);
    b[1] = '\0';
    term_write(cout, "Reading: ");
    term_write(cout, CAST(native_string, b));
    term_writeline(cout);
    b[1] = '\0';

    file_set_to_absolute_position(f, 1021);
    read_file(f, b, 1);
    b[1] = '\0';
    term_write(cout, "Reading: ");
    term_write(cout, CAST(native_string, b));
    term_writeline(cout);

    file_set_to_absolute_position(f, 615);
    read_file(f, b, 1);
    b[1] = '\0';
    term_write(cout, "Reading: ");
    term_write(cout, CAST(native_string, b));
    term_writeline(cout);

    term_write(cout, "The position was...");
    term_write(cout, f->current_pos);
    term_writeline(cout);
    file_set_to_absolute_position(f, 1);
    term_write(cout, "The position is now...");
    term_write(cout, f->current_pos);
    term_writeline(cout);

    read_file(f, b, 1);
    b[1] = '\0';
    term_write(cout, "Reading: ");
    term_write(cout, CAST(native_string, b));
    term_writeline(cout);

    } else {
      term_write(&tty, "\r\n Failed to open the program.\r\n");
    }
  }
  // } else {
  // term_write(cout, "Failed to write the file: ");
  // term_write(cout, err);
  // term_writeline(cout);
  // for(;;);
  // }
  term_run(tty);

  // Never exit, but never do anything either
  for (;;) thread::yield();

  return 0;
}

//-----------------------------------------------------------------------------
