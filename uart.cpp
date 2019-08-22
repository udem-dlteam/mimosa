
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
      break;
  }
}

/* TODO:
 * Add two args: arg1=stream de lecture arg2=stream d'ecriture
 */
error_code init_serial(int com_port) {
  error_code err = NO_ERROR;
  // La fonction init serial ne sera plus appelee de l'exterieur...

  if (!(com_port == COM1_PORT_BASE || com_port == COM2_PORT_BASE ||
        com_port == COM3_PORT_BASE || com_port == COM4_PORT_BASE)) {
    term_write(cout, CAST(void*, com_port));
    panic(L"Trying to init a serial port with an invalid COM port...");
  }

  outb(0x00, com_port + UART_8250_IER);  // Disable all interrupts
  outb(0x80, com_port + UART_8250_LCR);  // Enable DLAB (set baud rate divisor)
  outb(0x03, com_port + UART_8250_DLL);  // Set divisor to 3 (lo byte) 38400 baud
  outb(0x00, com_port + UART_8250_DLH);  // Set (high byte) 0 
  outb(0x03, com_port + UART_8250_LCR);  // 8 bits, no parity, one stop bit, close DLAB
  outb(0x0F, com_port + UART_8250_IER); 
  outb(0x8E, com_port + UART_8250_IIR);  //Do not enable FIFO, clear with 14-byte threshold
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

   return err;
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

  while (next_hi == port->rlo) {
    condvar_mutexless_wait(port->rd_cv);
  }

  if (next_hi != port->rlo) {
    port->rbuffer[port->rhi] = c;
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

void _handle_interrupt(uint16 port, uint8 com_index, uint8 iir) {
#ifdef SHOW_UART_MESSAGES
    debug_write("interrupt code:");
    debug_write(inb(port + UART_8250_IIR));
    debug_write("IRQ4 fired and COM ");
    debug_write(com_index);
    debug_write(" on port ");
    debug_write(port);
    debug_write(" got data");
#endif
  uint8 cause = UART_IIR_GET_CAUSE(iir);

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
      read_msr(port);
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
      handle_thr(port);
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
      read_lsr(port);
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
      read_RHR(port);  // ***
      break;
    case UART_IIR_TIMEOUT:
#ifdef SHOW_UART_MESSAGES
       debug_write("Timeout");
#endif
      // simple serial read
      read_RHR(port);
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
#ifdef SHOW_INTERRUPTS
  debug_write("\033[41m irq3 UART \033[0m");
#endif

  uint8 com2_iir = inb(COM2_PORT_BASE + UART_8250_IIR);
  uint8 com4_iir = inb(COM4_PORT_BASE + UART_8250_IIR);

  bool caught_something = FALSE;

  if (UART_IIR_PENDING(com2_iir)) {
    caught_something = TRUE;
    _handle_interrupt(COM2_PORT_BASE, 2, com2_iir);
  }
  if (UART_IIR_PENDING(com4_iir)) {
    caught_something = TRUE;
    _handle_interrupt(COM4_PORT_BASE, 4, com4_iir);
  }
  if (!(caught_something)) {
    panic(L"Misconfiguration of IRQ3.");
  }
}

void irq4() {
  ASSERT_INTERRUPTS_DISABLED();
  ACKNOWLEDGE_IRQ(4);

  // Interrupt 4 handles COM 1 and COM 3
#ifdef SHOW_INTERRUPTS
  debug_write("\033[41m irq4 UART \033[0m");
#endif

  uint8 com1_iir = inb(COM1_PORT_BASE + UART_8250_IIR);
  uint8 com3_iir = inb(COM3_PORT_BASE + UART_8250_IIR);

  bool caught_something = FALSE;

  if (UART_IIR_PENDING(com1_iir)) {
    caught_something = TRUE;
    _handle_interrupt(COM1_PORT_BASE, 1, com1_iir);
  }
  if (UART_IIR_PENDING(com3_iir)) {
    caught_something = TRUE;
    _handle_interrupt(COM3_PORT_BASE, 3, com3_iir);
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
  uart_handle->header.mode = mode;
  uart_handle->header._fs_header = NULL;  // TODO?
  uart_handle->header._vtable = &__uart_vtable;
  uart_handle->header.name = port_name;
  uart_handle->header.type = TYPE_VFILE;
  uart_handle->mode = mode;

  // TODO: PERFORM NULL CHECKS
  uart_handle->port = port_id;
  // Init the port data
  port->rbuffer = CAST(uint8*, kmalloc(sizeof(uint8) * COM_BUFFER_SIZE));
  port->wbuffer = CAST(uint8*, kmalloc(sizeof(uint8) * COM_BUFFER_SIZE));
  port->wbuffer_len = port->rbuffer_len = COM_BUFFER_SIZE;
  port->wrt_cv = CAST(condvar*, kmalloc(sizeof(condvar)));
  port->rd_cv = CAST(condvar*, kmalloc(sizeof(condvar)));
  new_condvar(port->wrt_cv);
  new_condvar(port->rd_cv);

  if (HAS_NO_ERROR(err)) {
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
  uart_file* f = CAST(uart_file*, f);
  com_port* port = &ports[com_num(f->port)];

  if (port->status & COM_PORT_STATUS_FORCIBLY_CLOSED) {
    return FNF_ERROR;
  }

  bool nonblock = f->mode & MODE_NONBLOCK_ACCESS;

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
      condvar_mutexless_signal(port->rd_cv);
    } else {
      break;
    }
  }

  enable_interrupts();

  if (HAS_NO_ERROR(err)) err = i;

  return err;
}

size_t uart_len(file* f) { return 0; }  // Makes no sense on a COM port

dirent* uart_readdir(DIR* dir) { return NULL; }  // Makes no sense on a COM port

static error_code detect_hardware() {
  error_code err = NO_ERROR;

  // Init the values to a known status
  for (uint8 i = 0; i < 4; ++i) {
    init_serial(com_num_to_port(i));
    ports[i].rbuffer = NULL;
    ports[i].wbuffer = NULL;
    ports[i].rbuffer_len = ports[i].wbuffer_len = 0;
    ports[i].rlo = ports[i].rhi = ports[i].wlo = ports[i].whi = 0;
    // Si le port est la, il faut mettre le status correct.
    if (port_exists(i)) ports[i].status |= COM_PORT_STATUS_EXISTS;
    ports[i].port = com_num_to_port(i);
  }

  return err;
}

// to detect if a port exists I can set a certain baud rate then watch if it has been
// correctly set on the receiving machine
bool port_exists(uint8 port_num){
  uint16 com_port = com_num_to_port(port_num);
  
  // set a baud rate of 57100
  outb(0x80, com_port + UART_8250_LCR);  // Enable DLAB (set baud rate divisor)
  outb(0x02, com_port + UART_8250_DLL); // set divisor to 2 div latch lo
  outb(0x00, com_port + UART_8250_DLH); // (now baud rate is 57100)
  uint8 LCR_val = inb(com_port + UART_8250_LCR) && 0x7F;
  outb( LCR_val, com_port + UART_8250_LCR); // disable DLAB
  
  // get the baud rate that we set
  outb(0x80, com_port + UART_8250_LCR); // enable DLAB
  uint8 divisor_latch = ( inb(com_port + UART_8250_DLH) >> 8 ) + inb(com_port + UART_8250_DLL );
  LCR_val = inb(com_port + UART_8250_LCR) && 0x7F;
  outb( LCR_val, com_port + UART_8250_LCR); // disable DLAB

  if( divisor_latch != 2 ) return false;

    // set a baud rate of 115200
  outb(0x80, com_port + UART_8250_LCR);  // Enable DLAB (set baud rate divisor)
  outb(0x01, com_port + UART_8250_DLL); // set divisor to 1 div latch lo
  outb(0x00, com_port + UART_8250_DLH); // (now baud rate is 115200)
  LCR_val = inb(com_port + UART_8250_LCR) && 0x7F;
  outb( LCR_val, com_port + UART_8250_LCR); // disable DLAB
  
  // get the baud rate that we set
  outb(0x80, com_port + UART_8250_LCR); // enable DLAB
  divisor_latch = ( inb(com_port + UART_8250_DLH) >> 8 ) + inb(com_port + UART_8250_DLL );
  LCR_val = inb(com_port + UART_8250_LCR) && 0x7F;
  outb( LCR_val, com_port + UART_8250_LCR); // disable DLAB

  return divisor_latch != 1;
}

error_code setup_uarts(vfnode* parent_node) {
  error_code err = NO_ERROR;

  if (ERROR(err = detect_hardware())) {
    return err;
  }

  __uart_vtable._file_close = uart_close_handle;
  __uart_vtable._file_len = uart_len;
  __uart_vtable._file_move_cursor = uart_move_cursor;
  __uart_vtable._file_read = uart_read;
  __uart_vtable._file_set_to_absolute_position = uart_set_abs_pos;
  __uart_vtable._file_write = uart_write;
  __uart_vtable._readdir = uart_readdir;

  if (ports[0].status & COM_PORT_STATUS_EXISTS) {
    new_vfnode(&COM1_NODE, COM1_NAME, TYPE_VFILE);
    COM1_NODE._value.file_gate.identifier = COM1_PORT_BASE;
    COM1_NODE._value.file_gate._vf_node_open = uart_open;
    vfnode_add_child(parent_node, &COM1_NODE);
  }

  if (ports[1].status & COM_PORT_STATUS_EXISTS) {
    new_vfnode(&COM2_NODE, COM2_NAME, TYPE_VFILE);
    COM2_NODE._value.file_gate.identifier = COM2_PORT_BASE;
    COM2_NODE._value.file_gate._vf_node_open = uart_open;
    vfnode_add_child(parent_node, &COM2_NODE);
  }

  if (ports[2].status & COM_PORT_STATUS_EXISTS) {
    new_vfnode(&COM3_NODE, COM3_NAME, TYPE_VFILE);
    COM3_NODE._value.file_gate.identifier = COM3_PORT_BASE;
    COM3_NODE._value.file_gate._vf_node_open = uart_open;
    vfnode_add_child(parent_node, &COM3_NODE);
  }

  if (ports[3].status & COM_PORT_STATUS_EXISTS) {
    new_vfnode(&COM4_NODE, COM4_NAME, TYPE_VFILE);
    COM4_NODE._value.file_gate.identifier = COM4_PORT_BASE;
    COM4_NODE._value.file_gate._vf_node_open = uart_open;
    vfnode_add_child(parent_node, &COM4_NODE);
  }

  return err;
}
