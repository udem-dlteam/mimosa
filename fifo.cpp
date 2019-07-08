// file: "fifo.cpp"

// Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 23 Oct 01  initial version (Marc Feeley)

//-----------------------------------------------------------------------------

#include "fifo.h"
#include "rtlib.h"
#include "thread.h"

//-----------------------------------------------------------------------------

// "fifo" class implementation.

#if 0

fifo::fifo ()
{
  _lo = 0;
  _hi = 0;
}

fifo::~fifo ()
{
}

#if 0

ssize_t fifo::write_or_timeout (uint8* buf, size_t count, time timeout)
{
  size_t done = 0;
  size_t n;

  if (count == 0)
    return 0;

#if 1
  if (!_m.lock_or_timeout (timeout))
    return 0;
#else
  _m.lock ();
#endif

  do
    {
      for (;;)
        {
          n = max_elements - (_hi + (max_elements+1) - _lo) % (max_elements+1);

          if (n > 0)
            break;

          if (!_put_cv.wait_or_timeout (&_m, timeout))
            return done;
        }

      if (n > count)
        n = count;

      size_t x = (max_elements+1) - _hi;

      if (x > n)
        x = n;

      if (x > 0)
        memcpy (_circularq + _hi, buf, x);

      if (n > x)
        memcpy (_circularq, buf + x, n - x);

      _hi = (_hi + n) % (max_elements+1);

      done += n;

      _get_cv.broadcast ();
    } while (done < count);

  _m.unlock ();

  return done;
}

ssize_t fifo::read_or_timeout (uint8* buf, size_t count, time timeout)
{
  size_t done = 0;
  size_t n;

  if (count == 0)
    return 0;

#if 1
  if (!_m.lock_or_timeout (timeout))
    return 0;
#else
  _m.lock ();
#endif

  do
    {
      for (;;)
        {
          n = (_hi + (max_elements+1) - _lo) % (max_elements+1);

          if (n > 0)
            break;

          if (!_get_cv.wait_or_timeout (&_m, timeout))
            return done;
        }

      if (n > count)
        n = count;

      size_t x = (max_elements+1) - _lo;

      if (x > n)
        x = n;

      if (x > 0)
        memcpy (buf, _circularq + _lo, x);

      if (n > x)
        memcpy (buf + x, _circularq, n - x);

      _lo = (_lo + n) % (max_elements+1);

      done += n;

      _put_cv.broadcast ();
    } while (done < count);

  _m.unlock ();

  return done;
}

#else

ssize_t fifo::write_or_timeout (uint8* buf, size_t count, time timeout)
{
  disable_interrupts ();

  for (;;)
    {
      int next_hi = (_hi + 1) % (max_elements+1);
      if (next_hi != _lo)
        {
          _circularq[_hi] = *buf;
          _hi = next_hi;
          break;
        }
      enable_interrupts ();
      if (less_time (timeout, current_time ()))
        return 0;
      //thread::yield ();
      disable_interrupts ();
    }

  enable_interrupts ();

  return 1;
}

ssize_t fifo::read_or_timeout (uint8* buf, size_t count, time timeout)
{
  disable_interrupts ();

  while (_lo == _hi)
    {
      enable_interrupts ();
      if (less_time (timeout, current_time ()))
        return 0;
      //thread::yield ();
      disable_interrupts ();
    }

  *buf = _circularq[_lo];

  _lo = (_lo + 1) % (max_elements+1);

  enable_interrupts ();

  return 1;
}

#endif

#else

#if 0

void fifo::put (uint8 byte)
{
  disable_interrupts ();

  for (;;)
    {
      int next_hi = (_hi + 1) % (max_elements+1);
      if (next_hi != _lo)
        {
          _circularq[_hi] = byte;
          _hi = next_hi;
          break;
        }
      enable_interrupts ();
      //thread::yield ();
      disable_interrupts ();
    }

  enable_interrupts ();
}

void fifo::get (uint8* byte)
{
  disable_interrupts ();

  while (_lo == _hi)
    {
      enable_interrupts ();
      //thread::yield ();
      disable_interrupts ();
    }

  *byte = _circularq[_lo];

  _lo = (_lo + 1) % (max_elements+1);

  enable_interrupts ();
}

bool fifo::get_or_timeout (uint8* byte, time timeout)
{
  disable_interrupts ();

  while (_lo == _hi)
    {
      enable_interrupts ();
      if (less_time (timeout, c1urrent_time ()))
        return FALSE;
      //thread::yield ();
      disable_interrupts ();
    }

  *byte = _circularq[_lo];

  _lo = (_lo + 1) % (max_elements+1);

  enable_interrupts ();

  return TRUE;
}

#else

#if 0

void fifo::put (uint8 byte)
{
  _m.lock ();

  for (;;)
    {
      int next_hi = (_hi + 1) % (max_elements+1);
      if (next_hi != _lo)
        {
          _circularq[_hi] = byte;
          _hi = next_hi;
          break;
        }
      _m.unlock ();
      //thread::yield ();
      _m.lock ();
    }

  _m.unlock ();
}

void fifo::get (uint8* byte)
{
  _m.lock ();

  while (_lo == _hi)
    {
      _m.unlock ();
      //thread::yield ();
      _m.lock ();
    }

  *byte = _circularq[_lo];

  _lo = (_lo + 1) % (max_elements+1);

  _m.unlock ();
}

bool fifo::get_or_timeout (uint8* byte, time timeout)
{
  _m.lock ();

  while (_lo == _hi)
    {
      _m.unlock ();
      if (less_time (timeout, current_time ()))
        return FALSE;
      //thread::yield ();
      _m.lock ();
    }

  *byte = _circularq[_lo];

  _lo = (_lo + 1) % (max_elements+1);

  _m.unlock ();

  return TRUE;
}

#else

void fifo_put(fifo *self, uint8 byte) {
  self->_m.lock();

  for (;;) {
    int next_hi = (self->_hi + 1) % (FIFO_MAX_ELEMENTS + 1);
    if (next_hi != self->_lo) {
      self->_circularq[self->_hi] = byte;
      self->_hi = next_hi;
      break;
    }
    // Wait until we get room
    self->_put_cv.wait(&self->_m);
  }
}

void fifo_get(fifo *self, uint8 *byte) {
  self->_m.lock();

  while (self->_lo == self->_hi) self->_get_cv.wait(&self->_m);

  *byte = self->_circularq[self->_lo];
  self->_lo = (self->_lo + 1) % (FIFO_MAX_ELEMENTS + 1);
  self->_put_cv.broadcast();
  self->_m.unlock();
}

bool fifo_get_or_timeout(fifo *self, uint8 *byte, time timeout) {
  self->_m.lock();

  while(self->_lo == self->_hi) {
    if(!self->_get_cv.wait_or_timeout(&self->_m, timeout)) {
      return FALSE;
    }
  }

  *byte = self->_circularq[self->_lo];
  self->_lo = (self->_lo + 1) % (FIFO_MAX_ELEMENTS + 1);
  self->_put_cv.broadcast();

  self->_m.unlock();

  return TRUE;
}

#endif

#endif

#endif

//-----------------------------------------------------------------------------

// Local Variables: //
// mode: C++ //
// End: //
