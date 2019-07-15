// file: "mouse.h"

// Copyright (c) 2001 by Marc Feeley and Université de Montréal, All
// Rights Reserved.
//
// Revision History
// 22 Sep 01  initial version (Marc Feeley)

#ifndef MOUSE_H
#define MOUSE_H

//-----------------------------------------------------------------------------

#include "general.h"
#include "intr.h"

//-----------------------------------------------------------------------------

// "mouse" class declaration.

class mouse
  {
  protected:

    mouse ();

    static mouse the_mouse;

    static void process_scancode (uint8 scancode);

    friend void irq12 ();
  };

//-----------------------------------------------------------------------------

#endif

// Local Variables: //
// mode: C++ //
// End: //
// file: "mouse.h"

// Copyright (c) 2001 by Marc Feeley and Université de Montréal, All
// Rights Reserved.
//
// Revision History
// 22 Sep 01  initial version (Marc Feeley)

#ifndef MOUSE_H
#define MOUSE_H

//-----------------------------------------------------------------------------

#include "general.h"

//-----------------------------------------------------------------------------

#endif

// Local Variables: //
// mode: C++ //
// End: //
