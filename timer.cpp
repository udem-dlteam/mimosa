// file: "timer.cpp"

// Copyright (c) 2001 by Marc Feeley and Université de Montréal, All
// Rights Reserved.
//
// Revision History
// 22 Sep 01  initial version (Marc Feeley)

//-----------------------------------------------------------------------------

#include "timer.h"
#include "asm.h"
#include "pic.h"
#include "pit.h"
#include "intr.h"
#include "term.h"
#include "thread.h"

//-----------------------------------------------------------------------------

void start_timer (int32 interval_in_usecs)
{
  int32 ticks = interval_in_usecs * PIT_COUNTS_PER_SEC / 1000000;

  ticks = interval_in_usecs;/////////////////

  if (ticks <= 0)
    ticks = 1;
  else if (ticks >= 0x10000)
    ticks = 0;

  outb (ticks & 0xff, PIT_PORT_CTR(0,PIT1_PORT_BASE)); // send LSB
  outb ((ticks >> 8) & 0xff, PIT_PORT_CTR(0,PIT1_PORT_BASE)); // send MSB
}

#ifdef USE_IRQ0_FOR_TIMER

void irq0 ()
{
  ACKNOWLEDGE_IRQ(0);

  start_timer (1);

  thread::yield ();
}

#endif

void setup_timer ()
{
#ifdef USE_IRQ0_FOR_TIMER

  outb (PIT_CW_CTR(0) | PIT_CW_LSB_MSB | PIT_CW_MODE(0),
        PIT_PORT_CW(PIT1_PORT_BASE));

  //start_timer (20);
  start_timer (0x10000);

#endif
}

//-----------------------------------------------------------------------------

// Local Variables: //
// mode: C++ //
// End: //
