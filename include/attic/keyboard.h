// file: "keyboard.h"

// Copyright (c) 2001 by Marc Feeley and Université de Montréal, All
// Rights Reserved.
//
// Revision History
// 22 Sep 01  initial version (Marc Feeley)

#ifndef KEYBOARD_H
#define KEYBOARD_H

//-----------------------------------------------------------------------------

#include "general.h"
#include "intr.h"

//-----------------------------------------------------------------------------

// "keyboard" class declaration.

class keyboard
  {
  protected:

    keyboard ();

    static keyboard the_keyboard;

    static void process_scancode (uint8 scancode);

    static void keypress (uint8 ch);

    struct scancode_def { uint16 normal, with_shift, with_ctrl, with_alt; };

    static scancode_def keycode_table[];
    static uint32 keymap[];

    friend void irq1 ();
  };

//-----------------------------------------------------------------------------

#endif

// Local Variables: //
// mode: C++ //
// End: //
