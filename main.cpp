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

int main ()
{ 
  term_c tty = new_term(0, 320, 80, 10, &font::mono_6x9, L"tty", true);

  // tty << "\033[46m" << "Gambit v4.9.3" << "\033[0m\n";
  // tty << "\n";
  // tty << "> ";

  // for (int i=0; i<20000; i++)
  //   for (int j=0; j<1000000; j++) ; // waste time

  // tty << "fooled you twice!!!!";

  for (;;) ; // loop forever!

  return 0;
}

//-----------------------------------------------------------------------------
