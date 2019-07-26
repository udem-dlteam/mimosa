
#include "asm.h"
#include "general.h"
#include "uart.h"
#include "intr.h"
#include "term.h"
#include "rtlib.h"

/**
 * Source for writing 8250 UART drivers can be found at
 * https://en.wikibooks.org/wiki/Serial_Programming/8250_UART_Programming
 * 
 */
void init_serial(int com_port) {

  if (!(com_port == COM1_PORT_BASE || com_port == COM2_PORT_BASE ||
        com_port == COM3_PORT_BASE || com_port == COM4_PORT_BASE)) {
    fatal_error("Trying to init a serial port with an invalid COM port...");
  }

  outb(0x00, com_port + UART_8250_IER);  // Disable all interrupts
  outb(0x80, com_port + UART_8250_LCR);  // Enable DLAB (set baud rate divisor)
  outb(0x03,
       com_port + UART_8250_DLL);  // Set divisor to 3 (lo byte) 38400 baud
  outb(0x00,
       com_port + UART_8250_DLM);  // Set divisor to 3 (lo byte) 38400 baud
  outb(0x03, com_port + UART_8250_LCR);  // 8 bits, no parity, one stop bit
  outb(0x01, com_port + UART_8250_IER);  //                  (hi byte)
  outb(0xC7,
       com_port +
           UART_8250_IIR);  // Enable FIFO, clear them, with 14-byte threshold
  outb(0x08, com_port + UART_8250_MCR);  // IRQs enabled, RTS/DSR set

  switch (com_port) {
    case COM1_PORT_BASE:
      ENABLE_IRQ(COM1_IRQ);
      break;
    case COM2_PORT_BASE:
      ENABLE_IRQ(COM2_IRQ);
      break;
    case COM3_PORT_BASE:
      ENABLE_IRQ(COM3_IRQ);
      break;
    case COM4_PORT_BASE:
      ENABLE_IRQ(COM4_IRQ);
      break;
   }
}


void _handle_interrupt(int com_index, uint8 iir) {
  term_write(cout, "\n\rIRQ4 fired and COM ");
  term_write(cout, com_index);
  term_write(cout, " got data\n\r");

  uint8 cause = UART_IIR_GET_CAUSE(iir);

  switch (cause) {
    case UART_IIR_MODEM:
      break;
    case UART_IIR_TRANSMITTER_HOLDING_REG:
      break;
    case UART_IIR_DATA_AVAIL:
        term_write(cout, "DATA AVAIL!");
      break;
    case UART_IIR_RCV_LINE:
      break;
    case UART_IIR_TIMEOUT:
      break;

    default:
      fatal_error("Illegal UART interrupt cause");
      break;
  }
}


#ifdef USE_IRQ4_FOR_UART

void irq3() {
  ACKNOWLEDGE_IRQ(3);

  // Interrupt 4 handles COM 2 and COM 4
#ifdef SHOW_INTERRUPTS
  term_write(cout, "\033[41m irq3 UART \033[0m");
#endif
  
  uint8 com2_iir = inb(COM2_PORT_BASE + UART_8250_IIR);
  uint8 com4_iir = inb(COM4_PORT_BASE + UART_8250_IIR);

  if (UART_IIR_PENDING(com2_iir)) {
    _handle_interrupt(2, com2_iir);
  } else if (UART_IIR_PENDING(com4_iir)) {
    _handle_interrupt(4, com4_iir);
  } else {
    fatal_error("Misconfiguration of IRQ3.");
  }
}

void irq4() {
  ACKNOWLEDGE_IRQ(4);

  // Interrupt 4 handles COM 1 and COM 3
#ifdef SHOW_INTERRUPTS
  term_write(cout, "\033[41m irq4 UART \033[0m");
#endif

  uint8 com1_iir = inb(COM1_PORT_BASE + UART_8250_IIR);
  uint8 com3_iir = inb(COM3_PORT_BASE + UART_8250_IIR);

  if (UART_IIR_PENDING(com1_iir)) {
    _handle_interrupt(1, com1_iir);
  } else if (UART_IIR_PENDING(com3_iir)) {
    _handle_interrupt(3, com3_iir);
  } else {
    fatal_error("Misconfiguration of IRQ4.");
  }
}
#endif



void send_serial(int port, char* x) {
  while (*x != '\0') {
    while ((inb(port + UART_8250_LSR) & UART_8250_LSR_THRE) == 0);
    outb(*x, port);
    x++;
  }
}

char read_serial(int com_port) {
  // while ((inb(com_port + UART_8250_LSR) & UART_8250_LSR_DR) == 0);
  // return inb(com_port);
}
