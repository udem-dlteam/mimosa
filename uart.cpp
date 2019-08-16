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

static native_string COM1_NAME = "ttys0";
static native_string COM2_NAME = "ttys1";
static native_string COM3_NAME = "ttys2";
static native_string COM4_NAME = "ttys3";

static vfnode COM1_NODE, COM2_NODE, COM3_NODE, COM4_NODE;

static file_vtable __uart_vtable;

static error_code uart_move_cursor(file* f, int32 mvmt);
static error_code uart_set_abs_pos(file* f, uint32 pos);
static error_code uart_close_handle(file* f);
static error_code uart_write(file* f, void* buff, uint32 count);
static error_code uart_read(file* f, void* buff, uint32 count);
static size_t uart_len(file* f);
static dirent* uart_readdir(DIR* dir);

int port_in_use = 0;

struct com_table{
  file* input;
  file* output;
};

uint8 com_num(int hex){
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
  
struct com_table com_tab[4];
//static bool errmess = TRUE;
#define errmess TRUE

/* TODO:
 * Add two args: arg1=stream de lecture arg2=stream d'ecriture
 */
void init_serial(int com_port, file* input, file* output) {
  /*
  Quoi faire!

  Voici l'idee generale. On veut que mimosa soit controller par le port serial. En ce moment, dans mimosa, le systeme
  a deux facons d'interagir avec l'utilisateur: STDIN, pour standard input et STDOUT, pour standard output. C'est un
  peu a la *nix. STDIN, en general, c'est le clavier. STDOUT, c'est la console. 

  L'idee, c'est de modifier mimosa pour que le port serial puisse aussi etre une entree standard de mimosa. Tu dois te dire, "fuck sa va
  etre l'enfer". Et ben non, j'ai tout fait ca beau pour que ce soit facile!

  STDOUT et STDIN sont deux streams. En gros, ce sont des trucs en memoire qui peuvent etre lu et ecrits par des objets dans le systeme.
  En ce moment, tout le input du systeme passe deja par STDIN. Si tu suis, tu devrais comprendre qu'il suffit d'ecrire dans STDIN, et le 
  systeme va etre capable de voir ce qui a ete ecrit. Pour STDOUT, il reste encore un peu de travail... Mais ca, c'est ma job. Tu peux quand meme
  lire / ecrire dans STDOUT.

  On a abstrait (comme dans *nix) les streams (et bien d'autre choses) dans des fichiers. Pour toi, il suffit d'utiliser les methodes d'ecriture
  et de lecture dans un fichier, et le reste est transparent..

  Voici ta mission. On veut etre capable d'initaliser un port COM pour qu'il se branche sur des streams. Des streams quelquonques! Dans la fonction
  init_serial, il va falloir ajouter deux arguments: le premier va etre le stream de lecture, c'est a dire ce qu'on envoit comme information
  dans le port COM et le deuxieme va etre le stream d'ecriture, c'est a dire ce qu'on recoit du port COM.

  Si tu as bien suivit, les deux arguments que tu recoit ne vont pas etre de type stream, mais bien de type file*. Cela va permettre 
  les trucs de communication a n'importe quoi. Si on veut que UART ecrive dans un fichier texte, on pourra le faire. Pour tester par contre,
  il faudra que tu prenne vraiment STDIN (tu peux aller voir dans ps2.cpp, ca ouvre deja STDIN et STDOUT c'est similaire).

  En resume:
    -  STDIN est un fichier, STDOUT aussi
    -  Tu ecrit dans STDIN
    -  Tu lis STDOUT (tu va lire du vide pour l'instant, alors commence par STDIN)
    -  vfs.h contient les routines de fichier. Les methodes de fichier sont des macros, mais tu as pas besoin
       de necessairement comprendre ce qui se passe, tu peux voir les definitions dans le file_vtable (la structure),
       les types d'arguments sont la, mais tu peux te baser sur ps2.cpp. 

  Une petite particularite: Les caracteres lus du clavier sont des int et non pas des chars. Alors quand tu ecrit dans STDIN, il
  faut que tu fasse int i = c;... file_write(stdin, &i, sizeof(i)). Inversement, quand tu lis de STDOUT, tu fais:

  int i;
  file_read(stdout, &i, sizeof(int));
  char c = 0xFF & i;

  Je pense que tu devrais etre pas pire avec ca!
  
   */
  port_in_use = com_port;
  com_tab[com_num(com_port)].input = input;
  com_tab[com_num(com_port)].output = output;

  if (!(com_port == COM1_PORT_BASE || com_port == COM2_PORT_BASE ||
        com_port == COM3_PORT_BASE || com_port == COM4_PORT_BASE)) {
    panic(L"Trying to init a serial port with an invalid COM port...");
  }

  outb(0x00, com_port + UART_8250_IER);  // Disable all interrupts
  outb(0x80, com_port + UART_8250_LCR);  // Enable DLAB (set baud rate divisor)
  outb(0x03,
       com_port + UART_8250_DLL);  // Set divisor to 3 (lo byte) 38400 baud
  outb(0x00,
       com_port + UART_8250_DLM);  // Set divisor to 3 (lo byte) 38400 baud
  outb(0x03, com_port + UART_8250_LCR);  // 8 bits, no parity, one stop bit
  outb(0x0F, com_port + UART_8250_IER);  //                  (hi byte)
  outb(0x8E,
       com_port +
           UART_8250_IIR);  //Do not enable FIFO, clear them, with 14-byte threshold
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


// Modem Status Register read
static void read_msr(uint16 port){
  uint8 c = inb(port + UART_8250_MSR);
  debug_write( "Read MSR");
  if( UART_MSR_CARRIER_DETECT(c) ){
    if(errmess)
      debug_write("Modem connected to another modem");
  } else {
    if(errmess)
      debug_write("Modem not connected to another modem");
  }
  //if( UART_MSR_RING_INDICATOR(c) ){
  //  debug_write("\r\nRing Voltage\r\n");
  //}
  if( UART_MSR_DATA_SET_READY(c) ){
    // TODO
    if(errmess)
      debug_write("Data Set Ready");
  }
  if( UART_MSR_CLEAR_TO_SEND(c) ){
    // handshaking signal. This is normally connected
    // to the RTS (Request To Send) signal on the remove
    // device. When that remote device asserts its RTS line,
    // data transmission can take place
    if(errmess)
      debug_write("Clear to Send");
  }
  // ignored bits for the moment
  //if( UART_MSR_DELTA_DATA_CARRIER_DETECT(c) ){}
  //if( UART_MSR_TRAILING_EDGE_RING_INDICATOR(c) ){}
  //if( UART_MSR_DELTA_DATA_SET_READY(c) ){}
  //if( UART_MSR_DELTA_CLEAR_TO_SEND(c) ){}
}

static void handle_thr(uint16 port){
  // bit 5 in LSR used to check if info must be written to THR or read from IIR
  if( UART_THR_GET_ACTION( inb( port + UART_8250_LSR ))){
    //TODO : if fifo is enabled , then more than one character can be written to THR
    //if(UART_IIR_GET_FIFO_STATE( inb( port + UART_IIR_FIFO_NO_FIFO ))){}  
    if(errmess)
      debug_write( "data wrote in THR");
  }
}

// TODO: write characters read in buffer
static void read_RHR(int com_port) {
  while (!(inb(com_port + UART_8250_LSR) & UART_8250_LSR_DR));
  //return inb(com_port);
  native_char c = (native_char)inb(com_port);
  unicode_char i = c;
  file_write(com_tab[com_num(com_port)].output, &i , sizeof(unicode_char));
  //debug_write( c);
}

static void read_lsr(uint16 port){
  uint8 e = inb(port + UART_8250_LSR);
  
  if( UART_LSR_DATA_AVAILABLE(e) ){
    //read (RHR)
    if(errmess)
      debug_write( "Data Available");
    read_RHR(port);
  }
  if( UART_LSR_OVERRUN_ERROR(e) ){
    if(errmess)
      debug_write( "OVERRUN_ERROR");
  }
  if( UART_LSR_PARITY_ERROR(e) ){
    if(errmess)
      debug_write( "PARITY_ERROR");
  }
  if( UART_LSR_FRAMING_ERROR(e) ){
    if(errmess)
      debug_write( "FRAMING_ERROR");
  }
  if( UART_LSR_BREAK_INTERRUPT(e) ){
    if(errmess)
      debug_write( "BREAK_INTERRUPT");
  }
  if( UART_LSR_CAN_RECEIVE(e) ){
    if(errmess)
      debug_write( "CAN_RECEIVE");
    // reading the lsr or writing to the data register clears this bit
  }
  if( UART_LSR_ALL_CAR_TRANSMITTED(e) ){
    if(errmess)
      debug_write( "ALL_CAR_TRANSMITTED");
  }
  //if( UART_LSR_ERROR_IN_RECEIVED_FIFO(e) ){
  //  //FIFO never used for the moment.
  //  debug_write( "ERROR IN RECEIVED FIFO,need to be cleared out");
  //}
}

void _handle_interrupt(uint16 port, uint8 com_index, uint8 iir) {
  if(errmess)
    debug_write( inb(port + UART_8250_IIR));
#ifdef SHOW_UART_MESSAGES
  if(errmess){
    debug_write( "IRQ4 fired and COM ");
    debug_write( com_index);
    debug_write( " on port ");
    debug_write( port);
    debug_write( " got data");
    }
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
    if(errmess)
      debug_write( "Read_modem_status_register");
    read_msr(port);
    break;
  case UART_IIR_TRANSMITTER_HOLDING_REG:
    // Transmitter empty
    // Caused by : The transmitter finishes sending
    //             data and is ready to accept additional data.
    // priority : next to lowest
    // Reading interrupt indentification register(IIR)
    // or writing to Transmit Holding Buffer (THR)
    if(errmess)
      debug_write( "Transmitter_Holding_reg");
    handle_thr(port);
    break;
  case UART_IIR_RCV_LINE:
    // Error or Break
    // caused by : Overrun error, parity error, framing
    //             error, or break interrupt.
    // priority : highest
    // reading line status register
    if(errmess)
      debug_write( "Error or Break");
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
    if(errmess)
      debug_write( "Data Avail");
    read_RHR(port); // ***
    break;
  case UART_IIR_TIMEOUT:
    if(errmess)
      debug_write( "Timeout");
    //simple serial read
    read_RHR(port);
    break;

  default:
    panic(L"Illegal UART interrupt cause");
    break;
  }
}

#ifdef USE_IRQ4_FOR_UART

void irq3() {
  ACKNOWLEDGE_IRQ(3);

  // Interrupt 4 handles COM 2 and COM 4
#ifdef SHOW_INTERRUPTS
  debug_write( "\033[41m irq3 UART \033[0m");
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
  if (!(caught_something)){
    panic(L"Misconfiguration of IRQ3.");
  }
}

void irq4() {
  ACKNOWLEDGE_IRQ(4);

  // Interrupt 4 handles COM 1 and COM 3
#ifdef SHOW_INTERRUPTS
  debug_write( "\033[41m irq4 UART \033[0m");
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
  if (!(caught_something)){
    panic(L"Misconfiguration of IRQ4.");
  }
}
#endif

// test bit 3 of LSR to determine if last character in THR was shifted
// out of the buffer before writing a character.
 //void send_serial(int port, char* x) {
 // while (*x != '\0') {
 //   while ((inb(port + UART_8250_LSR) & UART_8250_LSR_THRE) == 0);
 //   outb(*x, port);
 //   x++;
 // }
//}
void send_serial(int port, native_char x) {
   while ((inb(port + UART_8250_LSR) & UART_8250_LSR_THRE) == 0);
   outb(x, port);
}

error_code uart_move_cursor(file* f, int32 mvmt) {
  return ARG_ERROR; // Makes not sense on a COM port
}

error_code uart_set_abs_pos(file* f, uint32 pos) { 
  return ARG_ERROR; // Makes no sense on a COM port
}

error_code uart_close_handle(file* f) { return ARG_ERROR; }

error_code uart_write(file* f, void* buff, uint32 count) {return ARG_ERROR;}

error_code uart_read(file* f, void* buff, uint32 count) {return ARG_ERROR;}

size_t uart_len(file* f) {return 0;} // Makes no sense on a COM port

dirent* uart_readdir(DIR* dir) { return NULL; } // Makes no sense on a COM port

error_code setup_uarts(vfnode* parent_node) {
  __uart_vtable._file_close = uart_close_handle;
  __uart_vtable._file_len = uart_len;
  __uart_vtable._file_move_cursor = uart_move_cursor;
  __uart_vtable._file_read = uart_read;
  __uart_vtable._file_set_to_absolute_position = uart_set_abs_pos;
  __uart_vtable._file_write = uart_write;
  __uart_vtable._readdir = uart_readdir;

  // TODO: Il faut regarder si il y a vraiment 
  // les 4 disponibles. Si un port n'est pas la,
  // il ne faut pas le monter.

  new_vfnode(&COM1_NODE, COM1_NAME, TYPE_VFILE);
  COM1_NODE.header._vtable = &__uart_vtable;
  vfnode_add_child(parent_node, &COM1_NODE);

  new_vfnode(&COM2_NODE, COM2_NAME, TYPE_VFILE);
  COM2_NODE.header._vtable = &__uart_vtable;
  vfnode_add_child(parent_node, &COM2_NODE);

  new_vfnode(&COM3_NODE, COM3_NAME, TYPE_VFILE);
  COM3_NODE.header._vtable = &__uart_vtable;
  vfnode_add_child(parent_node, &COM3_NODE);

  new_vfnode(&COM4_NODE, COM4_NAME, TYPE_VFILE);
  COM4_NODE.header._vtable = &__uart_vtable;
  vfnode_add_child(parent_node, &COM4_NODE);

}
