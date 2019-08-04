// file: "pit.h"

// Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 22 Sep 01  initial version (Marc Feeley)

#ifndef __PIT_H
#define __PIT_H

//-----------------------------------------------------------------------------

#include "general.h"

//-----------------------------------------------------------------------------

//
// Definitions for 8254 programmable interval timer.
//

#define PIT1_PORT_BASE 0x40
#define PIT2_PORT_BASE 0x48

#define PIT_PORT_CTR(n,base) ((n)+(base))
#define PIT_PORT_CW(base) (3+(base))

#define PIT_CW_CTR(n)    ((n)<<6)
#define PIT_CW_READ_BACK (3<<6)
#define PIT_CW_CTR_LATCH (0<<4)
#define PIT_CW_LSB       (1<<4)
#define PIT_CW_MSB       (2<<4)
#define PIT_CW_LSB_MSB   (3<<4)
#define PIT_CW_MODE(n)   ((n)<<1)
#define PIT_CW_BCD       (1<<0)

#define PIT_COUNTS_PER_SEC 1193182 // clock frequency at input of PIT1 and PIT2

//-----------------------------------------------------------------------------

#endif

// Local Variables: //
// mode: C     //
// End: //
