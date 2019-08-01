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

  term_init(&tty, 0, 366, 80, 13, &font_mono_5x7, &font_mono_5x7, L"tty", TRUE);

  error_code err;

  file* f;

  __surround_with_debug_t("Creating file", {
    if (ERROR(err = create_file("", &f))) {
      term_write(&tty, "Error while creating an empty file");
      term_writeline(cout);
    } else {
      term_write(&tty, "Success while creating an empty file");
      term_writeline(cout);
    }
  });

  native_char buff[1024];
  for (int i = 0; i < 1024; ++i) {
    buff[i] = 'A';
  }
  buff[15] = 'B';
  buff[511] = 'C';
  buff[513] = 'D';
  buff[1021] = 'E';
  buff[615] = 'F';
  buff[1] = 'G';

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

    file_set_to_absolute_position(f, 15);
    read_file(f, b, 1);
    b[1] = '\0';
    term_write(cout, "Reading: ");
    term_write(cout, CAST(native_string, b));
    term_writeline(cout);

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

    close_file(f);
  }

  term_run(&tty);

  // Never exit, but never do anything either
  for (;;) {
    thread::yield();
  }

  return 0;
}

//-----------------------------------------------------------------------------
