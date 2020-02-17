#include "uart.h"
#include "asm.h"
#include "general.h"
#include "intr.h"
#include "rtlib.h"
#include "term.h"
#include "thread.h"
/**
 * Source for writing 8250 UART drivers can be found at
 * https://en.wikibooks.org/wiki/Serial_Programming/8250_UART_Programming
 */

native_string COM1_PATH = "/dev/ttys0";
native_string COM2_PATH = "/dev/ttys1";
native_string COM3_PATH = "/dev/ttys2";
native_string COM4_PATH = "/dev/ttys3";

static native_string COM1_NAME = "TTYS0";
static native_string COM2_NAME = "TTYS1";
static native_string COM3_NAME = "TTYS2";
static native_string COM4_NAME = "TTYS3";

static vfnode COM1_NODE, COM2_NODE, COM3_NODE, COM4_NODE;

static file_vtable __uart_vtable;

static error_code uart_move_cursor(file* f, int32 mvmt);
static error_code uart_set_abs_pos(file* f, uint32 pos);
static error_code uart_close_handle(file* f);
static error_code uart_write(file* f, void* buff, uint32 count);
static error_code uart_read(file* f, void* buff, uint32 count);
static size_t uart_len(file* f);
static dirent* uart_readdir(DIR* dir);

static com_port ports[4];


int port_in_use = 0;

static inline uint8 com_num(uint16 hex){
  switch (hex) {
    case COM1_PORT_BASE:
      return 0;
    case COM2_PORT_BASE:
      return 1;
    case COM3_PORT_BASE:
      return 2;
    case COM4_PORT_BASE:
      return 3;
    default:
      return 0;
  }
}

static inline uint16 com_num_to_port(uint8 num) {
  switch (num) {
    case 0:
      return COM1_PORT_BASE;
      break;
    case 1:
      return COM2_PORT_BASE;
      break;
    case 2:
      return COM3_PORT_BASE;
      break;
    case 3:
      return COM4_PORT_BASE;
      break;
    default:
      panic(L"Invalid port usage");
      return COM1_PORT_BASE;
      break;
  }
}

// Modem Status Register read
static void read_msr(uint16 port){
  uint8 c = inb(port + UART_8250_MSR);
  debug_write( "Read MSR");
  if( UART_MSR_CARRIER_DETECT(c) ){
#ifdef SHOW_UART_MESSAGES

      debug_write("Modem connected to another modem");
#endif
  } else {
#ifdef SHOW_UART_MESSAGES

      debug_write("Modem not connected to another modem");
#endif
  }
  //if( UART_MSR_RING_INDICATOR(c) ){
  //  debug_write("\r\nRing Voltage\r\n");
  //}
  if( UART_MSR_DATA_SET_READY(c) ){
    // TODO
#ifdef SHOW_UART_MESSAGES

      debug_write("Data Set Ready");
#endif
  }
  if( UART_MSR_CLEAR_TO_SEND(c) ){
    // handshaking signal. This is normally connected
    // to the RTS (Request To Send) signal on the remove
    // device. When that remote device asserts its RTS line,
    // data transmission can take place
#ifdef SHOW_UART_MESSAGES

      debug_write("Clear to Send");
#endif
  }
  // ignored bits for the moment
  //if( UART_MSR_DELTA_DATA_CARRIER_DETECT(c) ){}
  //if( UART_MSR_TRAILING_EDGE_RING_INDICATOR(c) ){}
  //if( UART_MSR_DELTA_DATA_SET_READY(c) ){}
  //if( UART_MSR_DELTA_CLEAR_TO_SEND(c) ){}
}

static void handle_thr(uint16 portn) {
  ASSERT_INTERRUPTS_DISABLED();
  // bit 5 in LSR used to check if info must be written to THR or read from IIR
  if (UART_THR_GET_ACTION(inb(portn + UART_8250_LSR))) {
    uint8 port_num_val = com_num(portn);
    com_port* port = &ports[port_num_val];

    if (port->wlo == port->whi) {
      port->status |= COM_PORT_STATUS_WRITE_READY;
    } else {
      port->status &= ~COM_PORT_STATUS_WRITE_READY;
      uint8 bt = port->wbuffer[port->wlo];
      outb(bt, portn + UART_8250_THR);
      port->wlo = (port->wlo + 1) % port->wbuffer_len;
      condvar_mutexless_signal(port->wrt_cv);
    }

#ifdef SHOW_UART_MESSAGES
     debug_write("data written in THR");
#endif
  }
}

// TODO: write characters read in buffer
static void read_RHR(int com_port) {
  while (!(inb(com_port + UART_8250_LSR) & UART_8250_LSR_DR))
    ;
  // return inb(com_port);
  uint8 c = inb(com_port + UART_8250_RHR);
  struct com_port_struct* port = &ports[com_num(com_port)];
  uint32 next_hi = (port->rhi + 1) % port->rbuffer_len;

  if (next_hi == port->rlo) {
    // MATHIEU: Ici, inserer le code pour dire au port UART: envoit plus rien pour l'instant
    return;
    // Tell the port that we cannot read right now
  }

  if (next_hi != port->rlo) {
    port->rbuffer[port->rhi] = c;
    condvar_mutexless_signal(port->rd_cv);
  } else {
    panic(L"[INT] Multiple writer?");
  }
  port->rhi = next_hi;
}

static void read_lsr(uint16 port) {
  uint8 e = inb(port + UART_8250_LSR);

  if (UART_LSR_DATA_AVAILABLE(e)) {
    // read (RHR)
#ifdef SHOW_UART_MESSAGES
     debug_write("Data Available");
#endif
    read_RHR(port);
  }
  if (UART_LSR_OVERRUN_ERROR(e)) {
#ifdef SHOW_UART_MESSAGES
     debug_write("OVERRUN_ERROR");
#endif
  }
  if (UART_LSR_PARITY_ERROR(e)) {
#ifdef SHOW_UART_MESSAGES
     debug_write("PARITY_ERROR");
#endif
  }
  if (UART_LSR_FRAMING_ERROR(e)) {
#ifdef SHOW_UART_MESSAGES
     debug_write("FRAMING_ERROR");
#endif
  }
  if (UART_LSR_BREAK_INTERRUPT(e)) {
#ifdef SHOW_UART_MESSAGES
     debug_write("BREAK_INTERRUPT");
#endif
  }
  if (UART_LSR_CAN_RECEIVE(e)) {
#ifdef SHOW_UART_MESSAGES
     debug_write("CAN_RECEIVE");
#endif
    // reading the lsr or writing to the data register clears this bit
  }
  if (UART_LSR_ALL_CAR_TRANSMITTED(e)) {
#ifdef SHOW_UART_MESSAGES
     debug_write("ALL_CAR_TRANSMITTED");
#endif
  }
  // if( UART_LSR_ERROR_IN_RECEIVED_FIFO(e) ){
  //  //FIFO never used for the moment.
  //  debug_write( "ERROR IN RECEIVED FIFO,need to be cleared out");
  //}
}

void _handle_interrupt(uint16 port_base, uint8 com_index, uint8 iir) {
#ifdef SHOW_UART_MESSAGES
    /* debug_write("interrupt code:"); */
    /* debug_write(inb(port + UART_8250_IIR)); */
    /* debug_write("IRQ4 fired and COM "); */
    /* debug_write(com_index); */
    /* debug_write(" on port "); */
    /* debug_write(port); */
    /* debug_write(" got data"); */
#endif
  uint8 cause = UART_IIR_GET_CAUSE(iir);
  com_port* port = &ports[com_num(port_base)];

  if (!(port->status & COM_PORT_STATUS_OPEN)) return;  // Drop silently

  switch (cause) {
    case UART_IIR_MODEM:
      // Modem Status
      // Caused by : Change in clear to send, data set
      //             ready, ring indicator, or received
      //             line signal detect signals.
      // priority :lowest
      // Reading Modem Status Register (MSR)
#ifdef SHOW_UART_MESSAGES
       debug_write("Read_modem_status_register");
#endif
      read_msr(port_base);
      break;
    case UART_IIR_TRANSMITTER_HOLDING_REG:
      // Transmitter empty
      // Caused by : The transmitter finishes sending
      //             data and is ready to accept additional data.
      // priority : next to lowest
      // Reading interrupt indentification register(IIR)
      // or writing to Transmit Holding Buffer (THR)
#ifdef SHOW_UART_MESSAGES
       debug_write("Transmitter_Holding_reg");
#endif
      handle_thr(port_base);
      break;
    case UART_IIR_RCV_LINE:
      // Error or Break
      // caused by : Overrun error, parity error, framing
      //             error, or break interrupt.
      // priority : highest
      // reading line status register
#ifdef SHOW_UART_MESSAGES
       debug_write("Error or Break");
#endif
      read_lsr(port_base);
      break;
    case UART_IIR_DATA_AVAIL:
      // Data Available
      // caused by : Data arriving from an external
      //             source in the Receive Register.
      // priority : next to highest
      // timeout is available on new model.
      // This means that we need to read data
      // before the connection timeouts
      // reading receive Buffer Register(RHR)
#ifdef SHOW_UART_MESSAGES
       debug_write("Data Avail");
#endif
      read_RHR(port_base);  // ***
      break;
    case UART_IIR_TIMEOUT:
#ifdef SHOW_UART_MESSAGES
       debug_write("Timeout");
#endif
      // simple serial read
      read_RHR(port_base);
      break;

    default:
      panic(L"Illegal UART interrupt cause");
      break;
  }
}

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

    // Interrupt 4 handles COM 1 and COM 3
    debug_write("\033[41m irq4 UART \033[0m");

    uint8 com1_iir = inb(COM1_PORT_BASE + UART_8250_IIR);
    uint8 com3_iir = inb(COM3_PORT_BASE + UART_8250_IIR);

    bool caught_something = FALSE;

    if (UART_IIR_PENDING(com1_iir)) {
        caught_something = TRUE;

#ifdef SHOW_UART_MESSAGES

        show_debug(com1_iir);

#endif
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

error_code uart_move_cursor(file* f, int32 mvmt) {
  return ARG_ERROR;  // Makes not sense on a COM port
}

error_code uart_set_abs_pos(file* f, uint32 pos) {
  return ARG_ERROR;  // Makes no sense on a COM port
}

error_code uart_open(uint32 id, file_mode mode, file** result) {
  error_code err = NO_ERROR;
  uint16 port_id = id & 0xFFFF;
  native_string port_name;

  switch (port_id) {
    case COM1_PORT_BASE:
      port_name = COM1_NAME;
      break;
    case COM2_PORT_BASE:
      port_name = COM2_NAME;
      break;
    case COM3_PORT_BASE:
      port_name = COM3_NAME;
      break;
    case COM4_PORT_BASE:
      port_name = COM4_NAME;
      break;
    default:
      return FNF_ERROR;
      break;
  }
  // should be removed but required at the moment for Gambit to execute properly
  mode |= MODE_NONBLOCK_ACCESS;

  // Only one can work on a port at a time
  com_port* port = &ports[com_num(port_id)];

  if (!(port->status & COM_PORT_STATUS_EXISTS)) {
    return FNF_ERROR;
  }

  if (port->status & COM_PORT_STATUS_OPEN) {
    return RESSOURCE_BUSY_ERR;
  }

  // TODO: check if the port is UP, if the port has not been opened, it's time
  // to open it
  // TODO: check the file mode, maybe it is incorrect?
  uart_file* uart_handle = CAST(uart_file*, kmalloc(sizeof(uart_file)));

  if (NULL == uart_handle) {
    err = MEM_ERROR;
  } else {
    uart_handle->header._fs_header = &__vfs;
    uart_handle->header.mode = mode;
    uart_handle->header._vtable = &__uart_vtable;
    uart_handle->header.name = port_name;
    uart_handle->header.type = TYPE_VFILE;
    uart_handle->mode = mode;
    uart_handle->port = port_id;
    // Init the port data
    if (NULL == (port->rbuffer =
                     CAST(uint8*, kmalloc(sizeof(uint8) * COM_BUFFER_SIZE)))) {
      err = MEM_ERROR;
    } else if (NULL ==
               (port->wbuffer =
                    CAST(uint8*, kmalloc(sizeof(uint8) * COM_BUFFER_SIZE)))) {
      err = MEM_ERROR;
    } else if (NULL ==
               (port->wrt_cv = CAST(condvar*, kmalloc(sizeof(condvar))))) {
      err = MEM_ERROR;
    } else if (NULL ==
               (port->rd_cv = CAST(condvar*, kmalloc(sizeof(condvar))))) {
      err = MEM_ERROR;
    }
  }

  if (HAS_NO_ERROR(err)) {
    port->wbuffer_len = port->rbuffer_len = COM_BUFFER_SIZE;
    new_condvar(port->wrt_cv);
    new_condvar(port->rd_cv);

    port->status |= COM_PORT_STATUS_OPEN;
    *result = CAST(file*, uart_handle);
  }

  return err;
}

error_code uart_close_handle(file* ff) {
  error_code err = NO_ERROR;
  uart_file* f = CAST(uart_file*, ff);
  com_port* port = &ports[com_num(f->port)];

  port->status &= ~COM_PORT_STATUS_OPEN;

  // Free the ressources
  port->rlo = port->rhi = port->whi = port->wlo = 0;
  port->wbuffer_len = port->rbuffer_len = 0;

  kfree(port->rbuffer);
  kfree(port->wbuffer);
  kfree(port->rd_cv);
  kfree(port->wrt_cv);

  return err;
}

error_code uart_write(file* ff, void* buff, uint32 count) {
  error_code err = NO_ERROR;
  uart_file* f = CAST(uart_file*, ff);
  uint16 port_base = f->port;
  com_port* port = &ports[com_num(port_base)];

  if (port->status & COM_PORT_STATUS_FORCIBLY_CLOSED) {
    return FNF_ERROR;
  }

  disable_interrupts();

  uint32 i = 0;
  // If the port is waiting, we need to feed it something
  // so coms can continue
  if (UART_THR_GET_ACTION(inb(port_base + UART_8250_LSR))) {
    if (port->status & COM_PORT_STATUS_WRITE_READY) {
      outb(CAST(uint8*, buff)[i++], port_base + UART_8250_THR);
    }
  }

  port->status &= ~COM_PORT_STATUS_WRITE_READY;

  for (; i < count; ++i) {
    uint32 next_hi = (port->whi + 1) % port->wbuffer_len;

    while (next_hi == port->wlo) {
      condvar_mutexless_wait(port->wrt_cv);
    }

    if (next_hi != port->wlo) {
      port->wbuffer[port->whi] = CAST(uint8*, buff)[i];
    } else {
      panic(L"Multiple writer?");
    }
    port->whi = next_hi;
    condvar_mutexless_signal(port->wrt_cv);
  }

  enable_interrupts();
  if (HAS_NO_ERROR(err)) err = i;

  return err;
}

error_code uart_read(file* ff, void* buff, uint32 count) {
  error_code err = NO_ERROR;
  uart_file* f = CAST(uart_file*, ff);
  com_port* port = &ports[com_num(f->port)];

  if (port->status & COM_PORT_STATUS_FORCIBLY_CLOSED) {
    return FNF_ERROR;
  }

  // bool nonblock = f->mode & MODE_NONBLOCK_ACCESS;
  bool nonblock = TRUE;

  disable_interrupts();

  uint32 i;
  for (i = 0; i < count; ++i) {
    while (!nonblock && port->rlo == port->rhi) {
      condvar_mutexless_wait(port->rd_cv);
    }

    if (port->rlo != port->rhi) {
      uint8 bt = port->rbuffer[port->rlo];
      CAST(uint8*, buff)[i] = bt;
      port->rlo = (port->rlo + 1) % port->rbuffer_len;
      // Mathieu: ici tu met le code pour envoyer au port UART que tu est pret a recevoir plus de caracteres
    } else {
      break;
    }
  }

  enable_interrupts();

  if (HAS_NO_ERROR(err)) {
    err = i;
  }

  return err;
}

size_t uart_len(file* f) { return 0; }  // Makes no sense on a COM port

dirent* uart_readdir(DIR* dir) { return NULL; }  // Makes no sense on a COM port
