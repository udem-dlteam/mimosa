#include "uart.h"
#include "asm.h"
#include "general.h"
#include "intr.h"
#include "rtlib.h"
#include "term.h"
#include "thread.h"

#ifdef USE_IRQ4_FOR_UART

void irq3() {
  ASSERT_INTERRUPTS_DISABLED();
  ACKNOWLEDGE_IRQ(3);

  // Interrupt 4 handles COM 2 and COM 4
  debug_write("\033[41m irq3 UART \033[0m");

  uint8 com2_iir = inb(COM2_PORT_BASE + UART_8250_IIR);
  uint8 com4_iir = inb(COM4_PORT_BASE + UART_8250_IIR);

  bool caught_something = FALSE;

  if (UART_IIR_PENDING(com2_iir)) {
    caught_something = TRUE;
    uint8 params[2] = {2, com2_iir};
    send_gambit_int(GAMBIT_UART_INT, params, 2);
  }

  if (UART_IIR_PENDING(com4_iir)) {
    caught_something = TRUE;
    uint8 params[2] = {4, com4_iir};
    send_gambit_int(GAMBIT_UART_INT, params, 2);
  }

  if (!(caught_something)) {
    panic(L"Misconfiguration of IRQ3.");
  }
}

void show_debug(uint8 iir) {
    uint8 cause = UART_IIR_GET_CAUSE(iir);
    switch (cause) {
        case UART_IIR_MODEM:
            debug_write("UART_IIR_MODEM");
            break;
        case UART_IIR_TRANSMITTER_HOLDING_REG:
            debug_write("UART_IIR_TRANSMITTER_HOLDING_REG");
            break;
        case UART_IIR_RCV_LINE:
            debug_write("UART_IIR_RCV_LINE");
            break;
        case UART_IIR_DATA_AVAIL:
            debug_write("UART_IIR_DATA_AVAIL");
            break;
        case UART_IIR_TIMEOUT:
            debug_write("UART_IIR_TIMEOUT");
            break;
        default:
            debug_write("Illegal UART interrupt cause");
            break;
    }
}

void irq4() {
    ASSERT_INTERRUPTS_DISABLED();
    ACKNOWLEDGE_IRQ(4);

    uint8 com1_iir = inb(COM1_PORT_BASE + UART_8250_IIR);
    uint8 com3_iir = inb(COM3_PORT_BASE + UART_8250_IIR);
    
    show_debug(com1_iir);

    bool caught_something = FALSE;

    if (UART_IIR_PENDING(com1_iir)) {
        caught_something = TRUE;
        uint8 params[2] = {1, com1_iir};
        send_gambit_int(GAMBIT_UART_INT, params, 2);
    }

    if (UART_IIR_PENDING(com3_iir)) {
        caught_something = TRUE;

        uint8 params[2] = {3, com3_iir};
        send_gambit_int(GAMBIT_UART_INT, params, 2);
    }

    if (!(caught_something)) {
        panic(L"Misconfiguration of IRQ4.");
    }
}
#endif
