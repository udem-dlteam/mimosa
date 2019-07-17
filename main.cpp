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
#include "ps2.h"

//-----------------------------------------------------------------------------

int main() {
  term* tty = &new_term(0, 320, 80, 10, &font_mono_6x9, L"tty", true);

  term_write(tty, "\033[46m");
  term_write(tty, "Gambit v4.9.3");
  term_write(tty, "\033[0m\n");

  term_write(tty, "\n");
  term_write(tty, "> ");

  for (int i = 0; i < 20000; i++)
    ;

  term_write(tty, "Video.cpp is in C!\n");

  for (;;);

  return 0;
}

//-----------------------------------------------------------------------------
