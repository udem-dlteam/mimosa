// file: "vga.h"

// Copyright (c) 2001 by Marc Feeley and Universit? de Montr?al, All
// Rights Reserved.
//
// Revision History
// 22 Sep 01  initial version (Marc Feeley)

#ifndef __VGA_H
#define __VGA_H

//-----------------------------------------------------------------------------

#include "general.h"

//-----------------------------------------------------------------------------

//
// Definitions for VGA graphics card.
//

#define VGA_PORT_SEQ_INDEX 0x3c4
#define VGA_PORT_SEQ_DATA 0x3c5

#define VGA_PORT_GRCTRL_INDEX 0x3ce
#define VGA_PORT_GRCTRL_DATA 0x3cf

#define VGA_MAP_MASK_REG 2
#define VGA_READ_MAP_SELECT_REG 4

//-----------------------------------------------------------------------------

#endif

// Local Variables: //
// mode: C++ //
// End: //
