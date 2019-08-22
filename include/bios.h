// file: "bios.h"

// Copyright (c) 2019 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.

#ifndef BIOS_H
#define BIOS_H

//-----------------------------------------------------------------------------

#include "general.h"

//-----------------------------------------------------------------------------

//
// Definitions for calling BIOS functions.
//

struct bios_call_regs {
  uint32 eax;
  uint32 ebx;
  uint32 ecx;
  uint32 edx;
  uint32 esi;
  uint32 edi;
};

extern void bios_call(int intr_num, struct bios_call_regs *regs);

//-----------------------------------------------------------------------------

#endif

// Local Variables: //
// mode: C++ //
// End: //
