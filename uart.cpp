
#include "asm.h"
#include "uart.h"
#include "intr.h"

void init_serial(int com_port) {
   outb(0x00, com_port + UART_8250_IER);    // Disable all interrupts
   outb(0x80, com_port + UART_8250_LCR);    // Enable DLAB (set baud rate divisor)
   outb(0x03, com_port + UART_8250_DLL);    // Set divisor to 3 (lo byte) 38400 baud
   outb(0x00, com_port + UART_8250_DLM);    // Set divisor to 3 (lo byte) 38400 baud
   outb(0x03, com_port + UART_8250_LCR);    // 8 bits, no parity, one stop bit
   outb(0x01, com_port + UART_8250_IER);    //                  (hi byte)
   outb(0xC7, com_port + UART_8250_IIR);    // Enable FIFO, clear them, with 14-byte threshold
   outb(0x08, com_port + UART_8250_MCR);    // IRQs enabled, RTS/DSR set

   ENABLE_IRQ(COM1_IRQ); // manually modify depending on which COM you choose
   ENABLE_IRQ(COM2_IRQ);
}

#ifdef USE_IRQ4_FOR_UART

void irq4 ()
{
#ifdef SHOW_INTERRUPTS
  term_write(cout, "\033[41m irq4 \033[0m");
#endif

  ACKNOWLEDGE_IRQ(4);

  

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
