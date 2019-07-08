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
#include "time.h"
#include "thread.h"

//-----------------------------------------------------------------------------

// "fifo" class declaration.

class fifo
{
public:
  fifo();
  virtual ~fifo();

  void put(uint8 byte);                           // write a byte to fifo
  void get(uint8 *byte);                          // read a byte from fifo
  bool get_or_timeout(uint8 *byte, time timeout); // like "get" but returns
                                                  // FALSE if timeout reached

protected:
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

  static const int max_elements = 200; // N in the picture above

  uint8 _circularq[max_elements + 1];
  volatile int _lo; // first byte that can be read
  volatile int _hi; // past last byte that was written

  mutex _m;        // to control access to the fifo's state
  condvar _put_cv; // to block "write" operations that can't complete
  condvar _get_cv; // to block "read" operations that can't complete
};

//-----------------------------------------------------------------------------

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

  volatile int _lo; // first byte that can be read
  volatile int _hi; // past last byte that was written
  mutex _m;         // to control access to the fifo's state
  condvar _put_cv;  // to block "write" operations that can't complete
  condvar _get_cv;  // to block "read" operations that can't complete
} fifo_struct;

void put(fifo_struct *self, uint8 byte);  // write a byte to fifo
void get(fifo_struct *self, uint8 *byte); // read a byte from fifo
bool get_or_timeout(fifo_struct *self, uint8 *byte, time timeout);
// like "get" but returns
// FALSE if timeout reached

#endif

// Local Variables: //
// mode: C++ //
// End: //
