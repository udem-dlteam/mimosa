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
#include "../drivers/filesystem/include/stdstream.h"

//-----------------------------------------------------------------------------
extern native_string COM1_PATH;
extern native_string COM2_PATH;
extern native_string COM3_PATH;
extern native_string COM4_PATH;

//
// Definitions for 8250/16550 universal asynchronous receiver transmitter.
//

/* DEFAULT BAUD RATE */
#define DEFAULT_BAUD_RATE 115200

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

//#define UART_16550_FCR 2
#define UART_8250_RHR 0
#define UART_8250_THR 0
#define UART_8250_IER 1
#define UART_8250_IIR 2
#define UART_8250_LCR 3
#define UART_8250_MCR 4
#define UART_8250_LSR 5
#define UART_8250_MSR 6
#define UART_8250_SCR 7

// to set the DLAB you have to set last LCR byte to 0 or 1

#define UART_8250_DLL 0 // divisor latch lo byte (needs DLAB at 0)
#define UART_8250_DLH 1 // divisor latch hi byte (needs DLAB at 1)



#define DIV_DLH(baud) ((115200 / (baud)) >> 8)
#define DIV_DLL(baud) ((115200 / (baud)) & 0xff)

// Line Status register (LSR)
#define UART_8250_LSR_ERF (1 << 7)
#define UART_8250_LSR_TEMT (1 << 6)
#define UART_8250_LSR_THRE (1 << 5)
#define UART_8250_LSR_BI (1 << 4)
#define UART_8250_LSR_FE (1 << 3)
#define UART_8250_LSR_PE (1 << 2)
#define UART_8250_LSR_OE (1 << 1)
#define UART_8250_LSR_DR (1 << 0)

// Line Status Register (LSR) interrupt cause 
#define UART_LSR_DATA_AVAILABLE(x) ((x) & UART_8250_LSR_DR)
#define UART_LSR_OVERRUN_ERROR(x) ((x) & UART_8250_LSR_OE)
#define UART_LSR_PARITY_ERROR(x) ((x) & UART_8250_LSR_PE)
#define UART_LSR_FRAMING_ERROR(x) ((x) & UART_8250_LSR_FE)
#define UART_LSR_BREAK_INTERRUPT(x) ((x) & UART_8250_LSR_BI)
#define UART_LSR_CAN_RECEIVE(x) ((x) & UART_8250_LSR_THRE)
#define UART_LSR_ALL_CAR_TRANSMITTED(x) ((x) & UART_8250_LSR_TEMT)
#define UART_LSR_ERROR_IN_RECEIVED_FIFO(x) ((x) & UART_8250_LSR_ERF)

// Modem Status Register (MSR) bit flags
#define UART_8250_MSR_CD (1 << 7)
#define UART_8250_MSR_RI (1 << 6)
#define UART_8250_MSR_DSR (1 << 5)
#define UART_8250_MSR_CTS (1 << 4)
#define UART_8250_MSR_DDCD (1 << 3)
#define UART_8250_MSR_TERI (1 << 2)
#define UART_8250_MSR_DDSR (1 << 1)
#define UART_8250_MSR_DCTS (1 << 0)

// Modem Status Register (MSR) interrupt causes
#define UART_MSR_CARRIER_DETECT(x) ((x) & UART_8250_MSR_CD)
#define UART_MSR_RING_INDICATOR(x) ((x) & UART_8250_MSR_RI)
#define UART_MSR_DATA_SET_READY(x) ((x) & UART_8250_MSR_DSR)
#define UART_MSR_CLEAR_TO_SEND(x) ((x) & UART_8250_MSR_CTS)
#define UART_MSR_DELTA_DATA_CARRIER_DETECT(x) ((x) & UART_8250_MSR_DDCD)
#define UART_MSR_TRAILING_EDGE_RING_INDICATOR(x) ((x) & UART_8250_MSR_TERI)
#define UART_MSR_DELTA_DATA_SET_READY(x) ((x) & UART_8250_MSR_DDSR)
#define UART_MSR_DELTA_CLEAR_TO_SEND(x) ((x) & UART_8250_MSR_DCTS)

#define UART_IIR_PENDING(x) (!((x) & (1 << 0)))
#define UART_IIR_IS_64_BIT_FIFO(x) ((x) & (1 << 5))
#define UART_IIR_GET_CAUSE(x) ((x & 0xE) >> 1)
#define UART_IIR_GET_FIFO_STATE(x) ((x & 0b11000000) >> 6)

#define UART_THR_GET_ACTION(x) ((x & (UART_8250_LSR_THRE)))

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
error_code setup_uarts(vfnode* parent_node);
void init_serial(int port, file* input, file* output);
void send_serial(int com, native_char x);
int serial_received(int com_port);
bool port_exists(uint8 port_num);

uint32 get_baud_rate(uint8 port_num);
void set_baud_rate(uint8 port_num, uint32 baud_rate);

error_code uart_open(uint32 id, file_mode mode, file** result);

typedef struct com_port_struct com_port;
typedef struct uart_file_struct uart_file;

struct uart_file_struct {
    file header;
    file_mode mode;
    uint16 port;
};

#define COM_BUFFER_SIZE 256

#define COM_PORT_STATUS_EXISTS (1 << 0)
#define COM_PORT_STATUS_OPEN (1 << 1)
#define COM_PORT_STATUS_FULL (1 << 2)
#define COM_PORT_STATUS_WAITING (1 << 3)
#define COM_PORT_STATUS_FORCIBLY_CLOSED (1 << 4)
#define COM_PORT_STATUS_READ_READY (1 << 5)
#define COM_PORT_STATUS_WRITE_READY (1 << 6)
#define COM_PORT_STATUS_RESERVED1 (1 << 7)

struct com_port_struct {
  uint16 port;
  uint32 rbuffer_len;
  uint32 wbuffer_len;
  volatile uint8 status; 
  volatile uint32 rlo, rhi, wlo, whi;
  uint8 *rbuffer, *wbuffer;
  condvar *wrt_cv;
  condvar *rd_cv;
};

#endif

// Local Variables: //
// mode: C++ //
// End: //
