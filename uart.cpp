
#include "asm.h"
#include "general.h"
#include "uart.h"
#include "intr.h"
#include "term.h"
#include "rtlib.h"

void init_serial(int com_port) {
  if (com_port < 1 || com_port > 4) {
    fatal_error("Illegal COM port value");
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
    case 1:
      ENABLE_IRQ(COM1_IRQ);
      break;
    case 2:
      ENABLE_IRQ(COM2_IRQ);
      break;
    case 3:
      ENABLE_IRQ(COM3_IRQ);
      break;
    case 4:
      ENABLE_IRQ(COM4_IRQ);
      break;
   }
}

#ifdef USE_IRQ4_FOR_UART

void irq3() {
#ifdef SHOW_INTERRUPTS
  term_write(cout, "\033[41m irq4 UART \033[0m");
#endif

  ACKNOWLEDGE_IRQ(3);
}

void irq4() {
#ifdef SHOW_INTERRUPTS
  term_write(cout, "\033[41m irq4 UART \033[0m");
#endif

  ACKNOWLEDGE_IRQ(4);
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

  while ((inb(com_port + UART_8250_LSR) & UART_8250_LSR_DR) == 0);
  return inb(com_port);
}
