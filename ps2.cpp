// file: "ps2.cpp"

// Copyright (c) 2002 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 20 Jan 02  merged support for ps/2 mouse (Marc Feeley)
// 22 Sep 01  initial version (Marc Feeley)

//-----------------------------------------------------------------------------

#include "asm.h"
#include "chrono.h"
#include "drivers/filesystem/include/stdstream.h"
#include "drivers/filesystem/include/vfs.h"
#include "intr.h"
#include "libc/include/libc_header.h"
#include "ps2.h"
#include "rtlib.h"
#include "term.h"
#include "thread.h"
#include "video.h"

//-----------------------------------------------------------------------------

static void controller_command(uint8 command) { outb(command, PS2_PORT_CMD); }

static void controller_config(uint8 mode) {
  controller_command(PS2_CMD_CONFIG);
  outb(mode, PS2_PORT_A);
}

static void controller_auxb_command(uint8 command) {
  controller_command(PS2_CMD_WRITE_MOUSE);
  outb(command, PS2_PORT_A);
}

static int32 controller_auxb_read() {
  int32 timeout = 800000; // 400000 works on our test machine, use twice
                          // as much for safety
  do {
    uint8 s = inb(PS2_PORT_STATUS);
    if (s & PS2_OUTB) {
      return inb(PS2_PORT_A);
    }
  } while (--timeout > 0);

  return -1;
}

//-----------------------------------------------------------------------------

#define DEAD 0

extern void send_signal(int sig); // from libc/src/signal.c

#ifdef USE_IRQ1_FOR_KEYBOARD

void irq1() {
#ifdef SHOW_INTERRUPTS
  term_write(cout, "\033[41m irq1 \033[0m");
#endif

  uint8 b = inb(PS2_PORT_A);
  uint8 params[1] = {b};
  if (!send_gambit_int(GAMBIT_KEYBOARD_INT, params, 1)) {
    // shrug
  }
  ACKNOWLEDGE_IRQ(1);
}

#endif

//-----------------------------------------------------------------------------

#ifdef ENABLE_MOUSE

static uint8 mouse_buf[PS2_MOUSE_BUFF_SIZE];
static uint8 mouse_buf_ptr = 0;

static void process_mouse_data(uint8 data) {
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

  if (mouse_buf_ptr >= PS2_MOUSE_BUFF_SIZE) {
    mouse_buf_ptr = mouse_buf_ptr % PS2_MOUSE_BUFF_SIZE;
  }

  mouse_buf[mouse_buf_ptr++] = data;

  if (mouse_buf_ptr < PS2_MOUSE_BUFF_SIZE) {
    return;
  }

  b1 = mouse_buf[0];
  b2 = mouse_buf[1];
  b3 = mouse_buf[2];

  dx = b2;
  dy = b3;

  if (b1 & (1 << 6))
    dx = 128;
  if (b1 & (1 << 7))
    dy = 128;
  if (b1 & (1 << 4))
    dx -= 256;
  if (b1 & (1 << 5))
    dy -= 256;

  video_move_mouse(&screen, dx, -dy);
}

#endif

#ifdef USE_IRQ12_FOR_MOUSE

void irq12() {
#ifdef SHOW_INTERRUPTS
  term_write(cout, "\033[41m irq12 \033[0m");
#endif

  ACKNOWLEDGE_IRQ(12);

#ifdef ENABLE_MOUSE
  process_mouse_data(inb(PS2_PORT_A));
#endif
}

//-----------------------------------------------------------------------------

error_code setup_ps2() {
  error_code err = NO_ERROR;
  term_write(cout, "Enabling PS2\n\r");

  controller_config(PS2_CONFIG_SCAN_CONVERT | PS2_CONFIG_ENABLE_IRQ12 |
                    PS2_CONFIG_ENABLE_IRQ1);

  controller_command(PS2_CMD_ENABLE_KEYBOARD);

#ifdef USE_IRQ1_FOR_KEYBOARD
  ENABLE_IRQ(1);
#endif

  controller_command(PS2_CMD_ENABLE_MOUSE);

  controller_auxb_command(PS2_MOUSE_CMD_RESET);

  if (controller_auxb_read() == PS2_MOUSE_ACK &&
      controller_auxb_read() == PS2_MOUSE_BAT_ACK && // the mouse passed BAT
      controller_auxb_read() == PS2_MOUSE_ID_PS2) {  // the mouse is a PS/2
    controller_auxb_command(PS2_MOUSE_CMD_ENABLE_REPORTING);

    if (controller_auxb_read() == PS2_MOUSE_ACK) {
#ifdef USE_IRQ12_FOR_MOUSE
      ENABLE_IRQ(12);
#endif
    }
  }

  return err;
}

#endif
//-----------------------------------------------------------------------------

// Local Variables: //
// mode: C++ //
// End: //
