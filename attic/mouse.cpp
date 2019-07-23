// file: "mouse.cpp"

// Copyright (c) 2002 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 20 Jan 02  initial version (Marc Feeley)

//-----------------------------------------------------------------------------

#include "mouse.h"
#include "asm.h"
#include "kbd.h"
#include "chrono.h"
#include "video.h"
#include "term.h"

//-----------------------------------------------------------------------------

static void controller_wait ()
{
  while (inb (KBD_PORT_STATUS) & KBD_INPB) ; // wait for controller to be ready
}

static void controller_command (uint8 command)
{
  controller_wait ();
  outb (command, KBD_PORT_CMD);
}

static void controller_config (uint8 mode)
{
  controller_command (KBD_CMD_CONFIG);
  controller_wait ();
  outb (mode, KBD_PORT_A);
}

static void controller_auxb_command (uint8 command)
{
  controller_command (KBD_CMD_WRITE_MOUSE);
  controller_wait ();
  outb (command, KBD_PORT_A);
}

static int32 controller_auxb_read ()
{
  int32 timeout = 800000; // 400000 works on our test machine, use twice
                          // as much for safety

  do {
    uint8 s = inb(KBD_PORT_STATUS);
    if (s & KBD_OUTB) {
      uint8 result = inb(KBD_PORT_A);
      if (s & KBD_AUXB) {
        term_write(cout, "got ");
        term_write(cout, result);
        term_write(cout, "\n");
        return result;
      }
      // a byte was received from the keyboard, but we are
      // expecting a byte from the auxb device, so ignore it
    }
  } while (--timeout > 0);

  return -1;
}

static int32 controller_auxb_command_and_read (uint8 command)
{
  controller_auxb_command (command);

  return controller_auxb_read ();
}

mouse::mouse ()
{
  controller_config
    (KBD_CONFIG_SCAN_CONVERT
     | KBD_CONFIG_SYSF
     | KBD_CONFIG_ENABLE_IRQ12
     | KBD_CONFIG_ENABLE_IRQ1);

  controller_command (KBD_CMD_ENABLE_MOUSE);

  if (controller_auxb_command_and_read (KBD_MOUSE_CMD_RESET)
      == KBD_MOUSE_ACK
      && controller_auxb_read () == KBD_MOUSE_BAT_ACK // the mouse's passed BAT
      && controller_auxb_read () == 0                 // the mouse's id is 0
      && controller_auxb_command_and_read (KBD_MOUSE_CMD_ENABLE_REPORTING)
      == KBD_MOUSE_ACK)
    {
#ifdef USE_IRQ12_FOR_MOUSE
      ENABLE_IRQ(12);
#endif
    }
}

uint8 mouse_buf[3];
uint8 mouse_buf_ptr = 2;
int32 mouse_x = 0;
int32 mouse_y = 0;

struct data
  {
    uint8 code;
    int us;
  } t[2400];
int n=0;

int mx=0,my=0;

void mouse::process_scancode (uint8 scancode)
{
  struct timeval tv;
  gettimeofday (&tv, NULL);
  static int xxx = 0;
  int xx = tv.tv_sec*1000000 + tv.tv_usec;
  int us = xx - xxx;
  xxx = xx;

  if (n%3 == 0 && (scancode & 8) == 0) return;

  if (n < 2400)
    {
      t[n].code = scancode;
      t[n].us = us;
      n++;
      if (n%3 == 0)
        {
          int dx = CAST(uint32,t[n-2].code);
          int dy = CAST(uint32,t[n-1].code);
          if (t[n-3].code & (1<<6)) dx = 128;
          if (t[n-3].code & (1<<7)) dy = 128;
          if (t[n-3].code & (1<<4)) dx -= 256;
          if (t[n-3].code & (1<<5)) dy -= 256;
          video::screen.move_mouse (dx, -dy);
          mx += dx; 
          my -= dy;
          //          cout << "(" << mx << "," << my << ") ";
        }
        if (n == 2400) {
          char str[9];
          int i;
          for (i = 0; i < n; i++) {

            if (i % 3 == 0) {
              term_write(cout, "\n");
            }
            
            scancode = t[i].code;
            str[8] = 0;
            str[0] = "01"[(scancode >> 7) & 1];
            str[1] = "01"[(scancode >> 6) & 1];
            str[2] = "01"[(scancode >> 5) & 1];
            str[3] = "01"[(scancode >> 4) & 1];
            str[4] = "01"[(scancode >> 3) & 1];
            str[5] = "01"[(scancode >> 2) & 1];
            str[6] = "01"[(scancode >> 1) & 1];
            str[7] = "01"[(scancode >> 0) & 1];

            term_write(cout, str);
            term_write(cout, " us=");
            term_write(cout, t[i].us);
            term_write(cout, "\n");
          }
        }
    }
  return;

  mouse_buf[mouse_buf_ptr++] = scancode;
  if (mouse_buf_ptr == 3) {
    int32 delta_x = 0;
    int32 delta_y = 0;
    delta_x = mouse_buf[1];
    if (mouse_buf[0] & (1 << 4)) delta_x -= 256;
    delta_y = mouse_buf[1];
    if (mouse_buf[0] & (1 << 5)) delta_y -= 256;
    mouse_x += delta_x;
    mouse_y += delta_y;
    if (delta_x != 0 || delta_y != 0) {
      term_write(cout, "(");
      term_write(cout, mouse_x);
      term_write(cout, " ");
      term_write(cout, mouse_y);
      term_write(cout, ")");
    }
    mouse_buf_ptr = 0;
  }
}

#ifdef USE_IRQ12_FOR_MOUSE

extern "C"
void irq12 ()
{
#ifdef SHOW_INTERRUPTS
  cout << "\033[41m irq12 \033[0m";
#endif

  ACKNOWLEDGE_IRQ(12);

  mouse::process_scancode (inb (KBD_PORT_A));
}

#endif

mouse mouse::the_mouse;

//-----------------------------------------------------------------------------

// Local Variables: //
// mode: C++ //
// End: //
