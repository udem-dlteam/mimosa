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

    if(ERROR(err = create_file("", NULL))) {
      term_write(tty, "Error while creating an empty file");
    } else {
      term_write(tty, "Success while creating an empty file");
    }
  });

  term_run(tty);

  // Never exit, but never do anything either
  for(;;) thread::yield();
  
  return 0;
}

//-----------------------------------------------------------------------------
