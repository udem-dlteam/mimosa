// file: "ps2.cpp"

// Copyright (c) 2002 by Marc Feeley and Université de Montréal, All
// Rights Reserved.
//
// Revision History
// 20 Jan 02  merged support for ps/2 mouse (Marc Feeley)
// 22 Sep 01  initial version (Marc Feeley)

//-----------------------------------------------------------------------------

#include "ps2.h"
#include "intr.h"
#include "asm.h"
#include "time.h"
#include "video.h"
#include "term.h"
#include "thread.h"

//-----------------------------------------------------------------------------

static void controller_command (uint8 command)
{
  outb (command, PS2_PORT_CMD);
}

static void controller_config (uint8 mode)
{
  controller_command (PS2_CMD_CONFIG);
  outb (mode, PS2_PORT_A);
}

static void controller_auxb_command (uint8 command)
{
  controller_command (PS2_CMD_WRITE_MOUSE);
  outb (command, PS2_PORT_A);
}

static int32 controller_auxb_read ()
{
  int32 timeout = 800000; // 400000 works on our test machine, use twice
                          // as much for safety

  do
    {
      uint8 s = inb (PS2_PORT_STATUS);
      if (s & PS2_OUTB)
        return inb (PS2_PORT_A);
    } while (--timeout > 0);

  return -1;
}

//-----------------------------------------------------------------------------

#define DEAD 0

struct scancode_def { uint16 normal, with_shift, with_ctrl, with_alt; };

static struct scancode_def keycode_table[] =
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

static uint32 keymap[4] = { 0, 0, 0, 0 };

#define PRESSED(code) (keymap[(code) >> 5] & (1 << ((code) & 0x1f)))

#define BUFFER_SIZE 16

static volatile unicode_char circular_buffer[BUFFER_SIZE];
static volatile int circular_buffer_lo = 0;
static volatile int circular_buffer_hi = 0;
static condvar* circular_buffer_cv;

static void keypress (uint8 ch)
{
  int next_hi = (circular_buffer_hi + 1) % BUFFER_SIZE;

  if (next_hi != circular_buffer_lo)
    {
      circular_buffer[circular_buffer_hi] = ch;
      circular_buffer_hi = next_hi;
      circular_buffer_cv->mutexless_signal ();
    }
}

unicode_char getchar ()
{
  disable_interrupts ();

  while (circular_buffer_lo == circular_buffer_hi)
    circular_buffer_cv->mutexless_wait ();

  unicode_char result = circular_buffer[circular_buffer_lo];

  circular_buffer_lo = (circular_buffer_lo + 1) % BUFFER_SIZE;

  circular_buffer_cv->mutexless_signal ();

  enable_interrupts ();

  return result;
}

static void process_keyboard_data (uint8 data)
{
  if (data >= KBD_SCANCODE_ESC
      && data <= KBD_SCANCODE_F12)
    {
      uint16 code;
      if (PRESSED(KBD_SCANCODE_LSHIFT)
          || PRESSED(KBD_SCANCODE_RSHIFT))
        code = keycode_table[data].with_shift;
      else if (PRESSED(KBD_SCANCODE_CTRL))
        code = keycode_table[data].with_ctrl;
      else if (PRESSED(KBD_SCANCODE_ALT))
        code = keycode_table[data].with_alt;
      else
        code = keycode_table[data].normal;
      keymap[data >> 5] |= (1 << (data & 0x1f));
      if (code != DEAD)
        keypress (code & 0xff);
    }
  else if (data >= (KBD_SCANCODE_ESC|0x80)
           && data <= (KBD_SCANCODE_F12|0x80))
    {
      data &= 0x7f;
      keymap[data >> 5] &= ~(1 << (data & 0x1f));
    }
}

#ifdef USE_IRQ1_FOR_KEYBOARD

void irq1 ()
{
#ifdef SHOW_INTERRUPTS
  cout << "\033[41m irq1 \033[0m";
#endif

  ACKNOWLEDGE_IRQ(1);

  process_keyboard_data (inb (PS2_PORT_A));
}

#endif

//-----------------------------------------------------------------------------

static uint8 mouse_buf[3];
static uint8 mouse_buf_ptr = 0;

static void process_mouse_data (uint8 data)
{
  uint32 b1;
  uint32 b2;
  uint32 b3;
  int32 dx;
  int32 dy;

  // PS/2 mouse packets contain 3 bytes with the following information:
  //
  // byte 1: bit 7 downto 0 ->  YO XO YS XS 1 MB RB LB
  // byte 2: low 8 bits of X motion
  // byte 3: low 8 bits of Y motion
  //
  // where, YO = 1 if the Y motion is too big, 0 otherwise
  //        XO = 1 if the X motion is too big, 0 otherwise
  //        YS = 1 if the Y motion is negative, 0 if it is nonnegative
  //        XS = 1 if the X motion is negative, 0 if it is nonnegative
  //        MB = 1 if middle button is pressed, 0 otherwise
  //        RB = 1 if right button is pressed, 0 otherwise
  //        LB = 1 if left button is pressed, 0 otherwise

  if (mouse_buf_ptr == 0  // make sure that first byte of packet has a 1
      && (data & 8) == 0) // at the right place (this helps
    return;               // resynchronize with the head of the mouse
                          // packets when communication errors occur)

  mouse_buf[mouse_buf_ptr++] = data;

  if (mouse_buf_ptr < 3)
    return;

  b1 = mouse_buf[0];
  b2 = mouse_buf[1];
  b3 = mouse_buf[2];

  dx = b2;
  dy = b3;

  if (b1 & (1<<6)) dx = 128;
  if (b1 & (1<<7)) dy = 128;
  if (b1 & (1<<4)) dx -= 256;
  if (b1 & (1<<5)) dy -= 256;

  video::screen.move_mouse (dx, -dy);
}

#ifdef USE_IRQ12_FOR_MOUSE

void irq12 ()
{
#ifdef SHOW_INTERRUPTS
  cout << "\033[41m irq12 \033[0m";
#endif

  ACKNOWLEDGE_IRQ(12);

  process_mouse_data (inb (PS2_PORT_A));
}

//-----------------------------------------------------------------------------

void setup_ps2 ()
{
  circular_buffer_cv = new condvar;

  controller_config
    (PS2_CONFIG_SCAN_CONVERT
     | PS2_CONFIG_ENABLE_IRQ12
     | PS2_CONFIG_ENABLE_IRQ1);

  controller_command (PS2_CMD_ENABLE_KEYBOARD);

#ifdef USE_IRQ1_FOR_KEYBOARD
  ENABLE_IRQ(1);
#endif

  controller_command (PS2_CMD_ENABLE_MOUSE);

  controller_auxb_command (PS2_MOUSE_CMD_RESET);

  if (controller_auxb_read () == PS2_MOUSE_ACK
      && controller_auxb_read () == PS2_MOUSE_BAT_ACK // the mouse passed BAT
      && controller_auxb_read () == PS2_MOUSE_ID_PS2) // the mouse is a PS/2
    {
      controller_auxb_command (PS2_MOUSE_CMD_ENABLE_REPORTING);

      if (controller_auxb_read () == PS2_MOUSE_ACK)
        {
#ifdef USE_IRQ12_FOR_MOUSE
          ENABLE_IRQ(12);
#endif
        }
    }
}

#endif

//-----------------------------------------------------------------------------

// Local Variables: //
// mode: C++ //
// End: //
