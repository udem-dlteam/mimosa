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
#include "uart.h"

int main() {

  term* tty = &new_term(0, 320, 80, 10, &font_mono_6x9, L"tty", true);

  term_write(cout, "INIT SERIAL\n");
  init_serial(COM1_PORT_BASE);
  //term_write(cout, "sending serial test to COM1_PORT_BASE\n");
  //send_serial(COM1_PORT_BASE, "HELLO WORLD");
  //term_write(cout, "sent\n");

  while(true){
    native_char c = read_serial(COM1_PORT_BASE);
    term_write(cout, (unsigned)c &0xff);
    term_write(cout, "\r\n");
    send_serial(COM1_PORT_BASE, "A\n");
  }
  
  term_write(cout, "Read");
  
  // Never exit, but never do anything either
  for(;;) thread::yield();
  
  return 0;
}

//-----------------------------------------------------------------------------
