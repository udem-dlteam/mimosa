// file: "kbd.h"

// Copyright (c) 2001 by Marc Feeley and Université de Montréal, All
// Rights Reserved.
//
// Revision History
// 22 Sep 01  initial version (Marc Feeley)

#ifndef __KBD_H
#define __KBD_H

//-----------------------------------------------------------------------------

#include "general.h"

//-----------------------------------------------------------------------------

//
// Definitions for 8042 keyboard controller.
//

#define KBD_PORT_A 0x60
#define KBD_PORT_B 0x61
#define KBD_PORT_C 0x62
#define KBD_PORT_CMD 0x64
#define KBD_PORT_STATUS 0x64

#define KBD_B_SW1 (1<<7) // port A reads switches or keyboard/mouse
#define KBD_B_TKT (1<<6) // keyboard clock enable
#define KBD_B_NME (1<<5) // enable NMI on expansion card error
#define KBD_B_NMI (1<<4) // enable NMI on memory parity check
#define KBD_B_REC (1<<3) // control cassette recorder motor
#define KBD_B_SW2 (1<<2) // select DIP switch to read
#define KBD_B_SPK (1<<1) // send PIT counter 2 to speaker/cassette
#define KBD_B_RSG (1<<0) // enable PIT counter 2

#define KBD_C_PAR (1<<7) // memory parity error
#define KBD_C_ERW (1<<6) // expansion card error
#define KBD_C_TIM (1<<5) // output of PIT counter 2
#define KBD_C_TON (1<<4) // cassette recorder signal
#define KBD_C_HS5 (1<<3) // DIP switch (HS6-HS2 = size of main memory)
#define KBD_C_HS4 (1<<2)
#define KBD_C_HS3 (1<<1)
#define KBD_C_HS2_HS6 (1<<0)

#define KBD_PARE (1<<7) // last byte received from kbd/mouse has parity error
#define KBD_TIM  (1<<6) // time-out error
#define KBD_AUXB (1<<5) // port A contains mouse data / keyboard data
#define KBD_KEYL (1<<4) // keyboard locked
#define KBD_C_D  (1<<3) // command / data byte
#define KBD_SYSF (1<<2) // system flag
#define KBD_INPB (1<<1) // input buffer (to controller) full
#define KBD_OUTB (1<<0) // output buffer (from keyboard/mouse) full

#define KBD_CMD_CONFIG           0x60
#define KBD_CMD_DISABLE_MOUSE    0xa7
#define KBD_CMD_ENABLE_MOUSE     0xa8
#define KBD_CMD_DISABLE_KEYBOARD 0xad
#define KBD_CMD_ENABLE_KEYBOARD  0xae
#define KBD_CMD_WRITE_MOUSE      0xd4

#define KBD_MOUSE_CMD_SET_SCALE11       0xe6
#define KBD_MOUSE_CMD_SET_SCALE21       0xe7
#define KBD_MOUSE_CMD_SET_RESOLUTION    0xe8
#define KBD_MOUSE_CMD_GET_STATUS        0xe9
#define KBD_MOUSE_CMD_SET_STREAM_MODE   0xea
#define KBD_MOUSE_CMD_GET_REPORT        0xeb
#define KBD_MOUSE_CMD_SET_REMOTE_MODE   0xf0
#define KBD_MOUSE_CMD_GET_ID            0xf2
#define KBD_MOUSE_CMD_SET_SAMPLE_RATE   0xf3
#define KBD_MOUSE_CMD_ENABLE_REPORTING  0xf4
#define KBD_MOUSE_CMD_DISABLE_REPORTING 0xf5
#define KBD_MOUSE_CMD_SET_DEFAULTS      0xf6
#define KBD_MOUSE_CMD_RESEND            0xfe
#define KBD_MOUSE_CMD_RESET             0xff

#define KBD_MOUSE_ACK      0xfa
#define KBD_MOUSE_BAT_ACK  0xaa
#define KBD_MOUSE_ID_PS2   0
#define KBD_MOUSE_ID_IMPS2 3

#define KBD_MOUSE_10_SAMPLES_PER_SEC  10
#define KBD_MOUSE_20_SAMPLES_PER_SEC  20
#define KBD_MOUSE_40_SAMPLES_PER_SEC  40
#define KBD_MOUSE_60_SAMPLES_PER_SEC  60
#define KBD_MOUSE_80_SAMPLES_PER_SEC  80
#define KBD_MOUSE_100_SAMPLES_PER_SEC 100
#define KBD_MOUSE_200_SAMPLES_PER_SEC 200

#define KBD_MOUSE_1_COUNTS_PER_MM 0
#define KBD_MOUSE_2_COUNTS_PER_MM 1
#define KBD_MOUSE_4_COUNTS_PER_MM 2
#define KBD_MOUSE_8_COUNTS_PER_MM 3

#define KBD_CONFIG_SCAN_CONVERT       (1<<6)
#define KBD_CONFIG_AUXB_CLOCK_DISABLE (1<<5)
#define KBD_CONFIG_KBD_CLOCK_DISABLE  (1<<4)
#define KBD_CONFIG_SYSF               (1<<2)
#define KBD_CONFIG_ENABLE_IRQ12       (1<<1)
#define KBD_CONFIG_ENABLE_IRQ1        (1<<0)

#define KBD_SCANCODE_ESC       0x01
#define KBD_SCANCODE_1         0x02
#define KBD_SCANCODE_2         0x03
#define KBD_SCANCODE_3         0x04
#define KBD_SCANCODE_4         0x05
#define KBD_SCANCODE_5         0x06
#define KBD_SCANCODE_6         0x07
#define KBD_SCANCODE_7         0x08
#define KBD_SCANCODE_8         0x09
#define KBD_SCANCODE_9         0x0A
#define KBD_SCANCODE_0         0x0B
#define KBD_SCANCODE_MINUS     0x0C
#define KBD_SCANCODE_EQUAL     0x0D
#define KBD_SCANCODE_BSPACE    0x0E
#define KBD_SCANCODE_TAB       0x0F
#define KBD_SCANCODE_Q         0x10
#define KBD_SCANCODE_W         0x11
#define KBD_SCANCODE_E         0x12
#define KBD_SCANCODE_R         0x13
#define KBD_SCANCODE_T         0x14
#define KBD_SCANCODE_Y         0x15
#define KBD_SCANCODE_U         0x16
#define KBD_SCANCODE_I         0x17
#define KBD_SCANCODE_O         0x18
#define KBD_SCANCODE_P         0x19
#define KBD_SCANCODE_LBRACK    0x1A
#define KBD_SCANCODE_RBRACK    0x1B
#define KBD_SCANCODE_ENTER     0x1C
#define KBD_SCANCODE_CTRL      0x1D
#define KBD_SCANCODE_A         0x1E
#define KBD_SCANCODE_S         0x1F
#define KBD_SCANCODE_D         0x20
#define KBD_SCANCODE_F         0x21
#define KBD_SCANCODE_G         0x22
#define KBD_SCANCODE_H         0x23
#define KBD_SCANCODE_J         0x24
#define KBD_SCANCODE_K         0x25
#define KBD_SCANCODE_L         0x26
#define KBD_SCANCODE_SEMICOLON 0x27
#define KBD_SCANCODE_QUOTE     0x28
#define KBD_SCANCODE_BQUOTE    0x29
#define KBD_SCANCODE_LSHIFT    0x2A
#define KBD_SCANCODE_BSLASH    0x2B
#define KBD_SCANCODE_Z         0x2C
#define KBD_SCANCODE_X         0x2D
#define KBD_SCANCODE_C         0x2E
#define KBD_SCANCODE_V         0x2F
#define KBD_SCANCODE_B         0x30
#define KBD_SCANCODE_N         0x31
#define KBD_SCANCODE_M         0x32
#define KBD_SCANCODE_COMMA     0x33
#define KBD_SCANCODE_PERIOD    0x34
#define KBD_SCANCODE_SLASH     0x35
#define KBD_SCANCODE_RSHIFT    0x36
#define KBD_SCANCODE_PRTSC     0x37
#define KBD_SCANCODE_ALT       0x38
#define KBD_SCANCODE_SPACE     0x39
#define KBD_SCANCODE_CAPS      0x3A
#define KBD_SCANCODE_F1        0x3B
#define KBD_SCANCODE_F2        0x3C
#define KBD_SCANCODE_F3        0x3D
#define KBD_SCANCODE_F4        0x3E
#define KBD_SCANCODE_F5        0x3F
#define KBD_SCANCODE_F6        0x40
#define KBD_SCANCODE_F7        0x41
#define KBD_SCANCODE_F8        0x42
#define KBD_SCANCODE_F9        0x43
#define KBD_SCANCODE_F10       0x44
#define KBD_SCANCODE_NUM       0x45
#define KBD_SCANCODE_SCRL      0x46
#define KBD_SCANCODE_HOME      0x47
#define KBD_SCANCODE_UP        0x48
#define KBD_SCANCODE_PGUP      0x49
#define KBD_SCANCODE_NUMMINUS  0x4A
#define KBD_SCANCODE_LEFT      0x4B
#define KBD_SCANCODE_CENTER    0x4C
#define KBD_SCANCODE_RIGHT     0x4D
#define KBD_SCANCODE_PLUS      0x4E
#define KBD_SCANCODE_END       0x4F
#define KBD_SCANCODE_DOWN      0x50
#define KBD_SCANCODE_PGDN      0x51
#define KBD_SCANCODE_INS       0x52
#define KBD_SCANCODE_DEL       0x53
#define KBD_SCANCODE_UNDEF1    0x54
#define KBD_SCANCODE_UNDEF2    0x55
#define KBD_SCANCODE_UNDEF3    0x56
#define KBD_SCANCODE_F11       0x57
#define KBD_SCANCODE_F12       0x58

#define KBD_SCANCODE_LWINDOW   0x5B // always prefixed with 0xE0
#define KBD_SCANCODE_RWINDOW   0x5C // always prefixed with 0xE0
#define KBD_SCANCODE_MENU      0x5D // always prefixed with 0xE0

//-----------------------------------------------------------------------------

#endif

// Local Variables: //
// mode: C++ //
// End: //
