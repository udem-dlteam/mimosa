// file: "fifo.h"

// Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 21 Oct 01  initial version (Marc Feeley)

#ifndef FIFO_H
#define FIFO_H

//-----------------------------------------------------------------------------

#include "general.h"
#include "chrono.h"
#include "thread.h"

//-----------------------------------------------------------------------------

// "fifo" class declaration.

const int FIFO_MAX_ELEMENTS = 200;

typedef struct fifo_struct
{
  // The fifo is implemented with a fixed length array accessed as a
  // circular queue.  The "_lo" and "_hi" pointers delimit the
  // section of the array that contains bytes that can be read.  The
  // "put" operation adds a byte at the "_hi" end and the "get"
  // operation removes a byte from the "_lo" end.
  //
  //                0   1   2   3   4   5  ... N-5 N-4 N-3 N-2 N-1  N
  //              +---+---+---+---+---+---+---+---+---+---+---+---+---+
  //  _circularq  | E | F | G |   |   |   |   |   |   | A | B | C | D |
  //              +---+---+---+---+---+---+---+---+---+---+---+---+---+
  //                            ^                       ^
  //                            |                       |
  //                           _hi                     _lo

  uint8 _circularq[FIFO_MAX_ELEMENTS + 1];

  volatile int _lo = 0; // first byte that can be read
  volatile int _hi = 0; // past last byte that was written
  mutex _m;         // to control access to the fifo's state
  condvar _put_cv;  // to block "write" operations that can't complete
  condvar _get_cv;  // to block "read" operations that can't complete
} fifo;

void fifo_put(fifo *self, uint8 byte);  // write a byte to fifo
void fifo_get(fifo *self, uint8 *byte); // read a byte from fifo
bool fifo_get_or_timeout(fifo *self, uint8 *byte, time timeout);
// like "get" but returns
// FALSE if timeout reached

#endif

// Local Variables: //
// mode: C     //
// End: //
