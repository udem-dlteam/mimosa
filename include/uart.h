// file: "uart.h"

// Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
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

/* COM1 */
#define COM1_PORT_BASE 0x3f8
#define COM1_IRQ 4
/* COM2 */
#define COM2_PORT_BASE 0x2f8
#define COM2_IRQ 3
/* COM3 */
#define COM3_PORT_BASE 0x3e8 
#define COM3_IRQ 4
 /* COM4 */
#define COM4_PORT_BASE 0x2e8
#define COM4_IRQ 3

#define UART_8250_RBR 0
#define UART_8250_THR 0
#define UART_8250_IER 1
#define UART_8250_IIR 2
#define UART_16550_FCR 2
#define UART_8250_LCR 3
#define UART_8250_MCR 4
#define UART_8250_LSR 5
#define UART_8250_MSR 6
#define UART_16550_SCR 7
#define UART_8250_DLL 0
#define UART_8250_DLM 1

#define DIV_MSB(baud) ((115200 / baud) >> 8)
#define DIV_LSB(baud) ((115200 / baud) & 0xff)

#define UART_8250_LSR_TEMT (1 << 6)
#define UART_8250_LSR_THRE (1 << 5)
#define UART_8250_LSR_BI (1 << 4)
#define UART_8250_LSR_FE (1 << 3)
#define UART_8250_LSR_PE (1 << 2)
#define UART_8250_LSR_OE (1 << 1)
#define UART_8250_LSR_DR (1 << 0)

#define UART_IIR_PENDING(x) (!((x) & (1 << 0)))
#define UART_IIR_IS_64_BIT_FIFO(x) ((x) & (1 << 5))
#define UART_IIR_GET_CAUSE(x) ((x & 0xE) >> 1)
#define UART_IIR_GET_FIFO_STATE(x) ((x & 0b11000000) >> 6)

// Interrupt Identification Register (IIR) interrupt cause
#define UART_IIR_MODEM 0
#define UART_IIR_TRANSMITTER_HOLDING_REG 1
#define UART_IIR_DATA_AVAIL 2
#define UART_IIR_RCV_LINE 3
#define UART_IIR_TIMEOUT 6
// Interrupt Identification Register (IIR) FIFO status
#define UART_IIR_FIFO_NO_FIFO 0
#define UART_IIR_FIFO_RESERVED 1
#define UART_IIR_FIFO_ENABLED_ERROR 2
#define UART_IIR_FIFO_ENABLED 3

//-----------------------------------------------------------------------------

void init_serial(int port);
void send_serial(int com, char *);
int serial_received(int com_port);
char read_serial(int com_port);

#endif

// Local Variables: //
// mode: C++ //
// End: //
