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
  
  __surround_with_debug_t("Create file", {
    error_code err;

    file* f;

    if(ERROR(err = create_file("", &f))) {
      term_write(tty, "Error while creating an empty file");
      term_writeline(cout);
    } else {
      term_write(tty, "Success while creating an empty file");
      term_writeline(cout);
    }

    native_string to_write = "This is the content of the file I am writing on disk";
    size_t n = strlen(to_write);
    if(ERROR(err = write_file(f, to_write, n + 1, TRUE))) {
      term_write(cout, "Failed to write the content to the file");
      term_writeline(cout);
    } else {
      term_write(cout, "Success writing! (wrote ");
      term_write(cout, n); term_write(cout, " characters).");
      term_writeline(cout);
    }

    close_file(f);

  });

  term_run(tty);

  // Never exit, but never do anything either
  for(;;) thread::yield();
  
  return 0;
}

//-----------------------------------------------------------------------------
