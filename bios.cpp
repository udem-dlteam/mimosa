// file: "bios.cpp"

// Copyright (c) 2019 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.

//-----------------------------------------------------------------------------

#include "bios.h"
#include "term.h"

//-----------------------------------------------------------------------------

void call_bios(struct bios_fn_regs *regs) {
  debug_write("enter call_bios");
  __asm__ __volatile__(
      "pusha                     \n \
       popa                      \n \
      "
      :
      :
      : "memory");
  debug_write("leave call_bios");
}

//-----------------------------------------------------------------------------

// mode: C++ //
// End: //
