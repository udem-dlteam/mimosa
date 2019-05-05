// file: "uart.h"

// Copyright (c) 2001 by Marc Feeley and Université de Montréal, All
// Rights Reserved.
//
// Revision History
// 22 Sep 01  initial version (Marc Feeley)

#ifndef __UART_H
#define __UART_H

//-----------------------------------------------------------------------------

#include "general.h"

//-----------------------------------------------------------------------------

//
// Definitions for 8250/16550 universal asynchronous receiver transmitter.
//

#define COM1_PORT_BASE 0x3f8
#define COM1_IRQ 4
#define COM2_PORT_BASE 0x2f8
#define COM2_IRQ 3

#define UART_8250_RBR  0
#define UART_8250_THR  0
#define UART_8250_IER  1
#define UART_8250_IIR  2
#define UART_16550_FCR 2
#define UART_8250_LCR  3
#define UART_8250_MCR  4
#define UART_8250_LSR  5
#define UART_8250_MSR  6
#define UART_16550_SCR 7
#define UART_8250_DLL  0
#define UART_8250_DLM  1

#define DIV_MSB(baud) ((115200/baud)>>8)
#define DIV_LSB(baud) ((115200/baud)&0xff)

#define UART_8250_LSR_TEMT (1<<6)
#define UART_8250_LSR_THRE (1<<5)
#define UART_8250_LSR_BI   (1<<4)
#define UART_8250_LSR_FE   (1<<3)
#define UART_8250_LSR_PE   (1<<2)
#define UART_8250_LSR_OE   (1<<1)
#define UART_8250_LSR_DR   (1<<0)

//-----------------------------------------------------------------------------

#endif

// Local Variables: //
// mode: C++ //
// End: //
