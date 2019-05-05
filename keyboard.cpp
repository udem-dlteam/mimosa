// file: "keyboard.cpp"

// Copyright (c) 2001 by Marc Feeley and Université de Montréal, All
// Rights Reserved.
//
// Revision History
// 22 Sep 01  initial version (Marc Feeley)

//-----------------------------------------------------------------------------

#include "keyboard.h"
#include "asm.h"
#include "kbd.h"
#include "term.h"

//-----------------------------------------------------------------------------

keyboard::keyboard ()
{
  outb (KBD_CMD_ENABLE_KEYBOARD, KBD_PORT_CMD);
  while (inb (KBD_PORT_STATUS) & KBD_INPB) ; // wait for controller to be ready

#ifdef USE_IRQ1_FOR_KEYBOARD
  ENABLE_IRQ(1);
#endif
}

#define DEAD 0

keyboard::scancode_def keyboard::keycode_table[] =
{
  { DEAD  , DEAD  , DEAD  , DEAD   } // not a valid scancode
, { 0x011b, 0x011b, 0x011b, 0x0100 } // KBD_SCANCODE_ESC
, { 0x0231, 0x0221, DEAD  , 0x7800 } // KBD_SCANCODE_1
, { 0x0332, 0x0340, 0x0300, 0x7900 } // KBD_SCANCODE_2
, { 0x0433, 0x0423, DEAD  , 0x7a00 } // KBD_SCANCODE_3
, { 0x0534, 0x0524, DEAD  , 0x7b00 } // KBD_SCANCODE_4
, { 0x0635, 0x0625, DEAD  , 0x7c00 } // KBD_SCANCODE_5
, { 0x0736, 0x075e, 0x071e, 0x7d00 } // KBD_SCANCODE_6
, { 0x0837, 0x0826, DEAD  , 0x7e00 } // KBD_SCANCODE_7
, { 0x0938, 0x092a, DEAD  , 0x7f00 } // KBD_SCANCODE_8
, { 0x0a39, 0x0a28, DEAD  , 0x8000 } // KBD_SCANCODE_9
, { 0x0b30, 0x0b29, DEAD  , 0x8100 } // KBD_SCANCODE_0
, { 0x0c2d, 0x0c5f, 0x0c1f, 0x8200 } // KBD_SCANCODE_MINUS
, { 0x0d3d, 0x0d2b, DEAD  , 0x8300 } // KBD_SCANCODE_EQUAL
, { 0x0e08, 0x0e08, 0x0e7f, DEAD   } // KBD_SCANCODE_BSPACE
, { 0x0f09, 0x0f00, DEAD  , DEAD   } // KBD_SCANCODE_TAB
, { 0x1071, 0x1051, 0x1011, 0x1000 } // KBD_SCANCODE_Q
, { 0x1177, 0x1157, 0x1117, 0x1100 } // KBD_SCANCODE_W
, { 0x1265, 0x1245, 0x1205, 0x1200 } // KBD_SCANCODE_E
, { 0x1372, 0x1352, 0x1312, 0x1300 } // KBD_SCANCODE_R
, { 0x1474, 0x1454, 0x1414, 0x1400 } // KBD_SCANCODE_T
, { 0x1579, 0x1559, 0x1519, 0x1500 } // KBD_SCANCODE_Y
, { 0x1675, 0x1655, 0x1615, 0x1600 } // KBD_SCANCODE_U
, { 0x1769, 0x1749, 0x1709, 0x1700 } // KBD_SCANCODE_I
, { 0x186f, 0x184f, 0x180f, 0x1800 } // KBD_SCANCODE_O
, { 0x1970, 0x1950, 0x1910, 0x1900 } // KBD_SCANCODE_P
, { 0x1a5b, 0x1a7b, 0x1a1b, DEAD   } // KBD_SCANCODE_LBRACK
, { 0x1b5d, 0x1b7d, 0x1b1d, DEAD   } // KBD_SCANCODE_RBRACK
, { 0x1c0d, 0x1c0d, 0x1c0a, DEAD   } // KBD_SCANCODE_ENTER
, { DEAD  , DEAD  , DEAD  , DEAD   } // KBD_SCANCODE_CTRL
, { 0x1e61, 0x1e41, 0x1e01, 0x1e00 } // KBD_SCANCODE_A
, { 0x1f73, 0x1f53, 0x1f13, 0x1f00 } // KBD_SCANCODE_S
, { 0x2064, 0x2044, 0x2004, 0x2000 } // KBD_SCANCODE_D
, { 0x2166, 0x2146, 0x2106, 0x2100 } // KBD_SCANCODE_F
, { 0x2267, 0x2247, 0x2207, 0x2200 } // KBD_SCANCODE_G
, { 0x2368, 0x2348, 0x2308, 0x2300 } // KBD_SCANCODE_H
, { 0x246a, 0x244a, 0x240a, 0x2400 } // KBD_SCANCODE_J
, { 0x256b, 0x254b, 0x250b, 0x2500 } // KBD_SCANCODE_K
, { 0x266c, 0x264c, 0x260c, 0x2600 } // KBD_SCANCODE_L
, { 0x273b, 0x273a, DEAD  , DEAD   } // KBD_SCANCODE_SEMICOLON
, { 0x2827, 0x2822, DEAD  , DEAD   } // KBD_SCANCODE_QUOTE
, { 0x2960, 0x297e, DEAD  , DEAD   } // KBD_SCANCODE_BQUOTE
, { DEAD  , DEAD  , DEAD  , DEAD   } // KBD_SCANCODE_LSHIFT
, { 0x2b5c, 0x2b7c, 0x2b1c, DEAD   } // KBD_SCANCODE_BSLASH
, { 0x2c7a, 0x2c5a, 0x2c1a, 0x2c00 } // KBD_SCANCODE_Z
, { 0x2d78, 0x2d58, 0x2d18, 0x2d00 } // KBD_SCANCODE_X
, { 0x2e63, 0x2e43, 0x2e03, 0x2e00 } // KBD_SCANCODE_C
, { 0x2f76, 0x2f56, 0x2f16, 0x2f00 } // KBD_SCANCODE_V
, { 0x3062, 0x3042, 0x3002, 0x3000 } // KBD_SCANCODE_B
, { 0x316e, 0x314e, 0x310e, 0x3100 } // KBD_SCANCODE_N
, { 0x326d, 0x324d, 0x320d, 0x3200 } // KBD_SCANCODE_M
, { 0x332c, 0x333c, DEAD  , DEAD   } // KBD_SCANCODE_COMMA
, { 0x342e, 0x343e, DEAD  , DEAD   } // KBD_SCANCODE_PERIOD
, { 0x352f, 0x353f, DEAD  , DEAD   } // KBD_SCANCODE_SLASH
, { DEAD  , DEAD  , DEAD  , DEAD   } // KBD_SCANCODE_RSHIFT
, { 0x372a, 0x372a, DEAD  , DEAD   } // KBD_SCANCODE_PRTSC
, { DEAD  , DEAD  , DEAD  , DEAD   } // KBD_SCANCODE_ALT
, { 0x3920, 0x3920, 0x3920, 0x3920 } // KBD_SCANCODE_SPACE
, { DEAD  , DEAD  , DEAD  , DEAD   } // KBD_SCANCODE_CAPS
, { 0x3b00, 0x5400, 0x5e00, 0x6800 } // KBD_SCANCODE_F1
, { 0x3c00, 0x5500, 0x5f00, 0x6900 } // KBD_SCANCODE_F2
, { 0x3d00, 0x5600, 0x6000, 0x6a00 } // KBD_SCANCODE_F3
, { 0x3e00, 0x5700, 0x6100, 0x6b00 } // KBD_SCANCODE_F4
, { 0x3f00, 0x5800, 0x6200, 0x6c00 } // KBD_SCANCODE_F5
, { 0x4000, 0x5900, 0x6300, 0x6d00 } // KBD_SCANCODE_F6
, { 0x4100, 0x5a00, 0x6400, 0x6e00 } // KBD_SCANCODE_F7
, { 0x4200, 0x5b00, 0x6500, 0x6f00 } // KBD_SCANCODE_F8
, { 0x4300, 0x5c00, 0x6600, 0x7000 } // KBD_SCANCODE_F9
, { 0x4400, 0x5d00, 0x6700, 0x7100 } // KBD_SCANCODE_F10
, { DEAD  , DEAD  , DEAD  , DEAD   } // KBD_SCANCODE_NUM
, { DEAD  , DEAD  , DEAD  , DEAD   } // KBD_SCANCODE_SCRL
, { 0x4700, 0x4737, 0x7700, DEAD   } // KBD_SCANCODE_HOME
, { 0x4800, 0x4838, DEAD  , DEAD   } // KBD_SCANCODE_UP
, { 0x4900, 0x4939, 0x8400, DEAD   } // KBD_SCANCODE_PGUP
, { 0x4a2d, 0x4a2d, DEAD  , DEAD   } // KBD_SCANCODE_NUMMINUS
, { 0x4b00, 0x4b34, 0x7300, DEAD   } // KBD_SCANCODE_LEFT
, { 0x4c00, 0x4c35, DEAD  , DEAD   } // KBD_SCANCODE_CENTER
, { 0x4d00, 0x4d36, 0x7400, DEAD   } // KBD_SCANCODE_RIGHT
, { 0x4e2b, 0x4e2b, DEAD  , DEAD   } // KBD_SCANCODE_PLUS
, { 0x4f00, 0x4f31, 0x7500, DEAD   } // KBD_SCANCODE_END
, { 0x5000, 0x5032, DEAD  , DEAD   } // KBD_SCANCODE_DOWN
, { 0x5100, 0x5133, 0x7600, DEAD   } // KBD_SCANCODE_PGDN
, { 0x5200, 0x5230, DEAD  , DEAD   } // KBD_SCANCODE_INS
, { 0x5300, 0x532e, DEAD  , DEAD   } // KBD_SCANCODE_DEL
, { DEAD  , DEAD  , DEAD  , DEAD   } // KBD_SCANCODE_UNDEF1
, { DEAD  , DEAD  , DEAD  , DEAD   } // KBD_SCANCODE_UNDEF2
, { DEAD  , DEAD  , DEAD  , DEAD   } // KBD_SCANCODE_UNDEF3
, { DEAD  , DEAD  , DEAD  , DEAD   } // KBD_SCANCODE_F11
, { DEAD  , DEAD  , DEAD  , DEAD   } // KBD_SCANCODE_F12
};

uint32 keyboard::keymap[4] = { 0, 0, 0, 0 };

#define PRESSED(code) (keymap[(code) >> 5] & (1 << ((code) & 0x1f)))

void keyboard::keypress (uint8 ch)
{
  native_char str[2];
  str[0] = ch;
  str[1] = '\0';
  cout << str;
}

void keyboard::process_scancode (uint8 scancode)
{
  if (scancode >= KBD_SCANCODE_ESC
      && scancode <= KBD_SCANCODE_F12)
    {
      uint16 code;
      if (PRESSED(KBD_SCANCODE_LSHIFT)
          || PRESSED(KBD_SCANCODE_RSHIFT))
        code = keycode_table[scancode].with_shift;
      else if (PRESSED(KBD_SCANCODE_CTRL))
        code = keycode_table[scancode].with_ctrl;
      else if (PRESSED(KBD_SCANCODE_ALT))
        code = keycode_table[scancode].with_alt;
      else
        code = keycode_table[scancode].normal;
      keymap[scancode >> 5] |= (1 << (scancode & 0x1f));
      if (code != DEAD)
        keypress (code & 0xff);
    }
  else if (scancode >= (KBD_SCANCODE_ESC|0x80)
           && scancode <= (KBD_SCANCODE_F12|0x80))
    {
      scancode &= 0x7f;
      keymap[scancode >> 5] &= ~(1 << (scancode & 0x1f));
    }
}

#ifdef USE_IRQ1_FOR_KEYBOARD

extern "C"
void irq1 ()
{
#ifdef SHOW_INTERRUPTS
  cout << "\033[41m irq1 \033[0m";
#endif

  ACKNOWLEDGE_IRQ(1);

  keyboard::process_scancode (inb (KBD_PORT_A));
}

#endif

keyboard keyboard::the_keyboard;

//-----------------------------------------------------------------------------

// Local Variables: //
// mode: C++ //
// End: //
