// file: "rtlib.cpp"

// Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 22 Sep 01  initial version (Marc Feeley)

//-----------------------------------------------------------------------------

#include "chrono.h"
#include "disk.h"
#include "drivers/filesystem/include/stdstream.h"
#include "drivers/filesystem/include/vfs.h"
#include "heap.h"
#include "ide.h"
#include "intr.h"
#include "libc/include/libc_header.h"
#include "ps2.h"
#include "rtlib.h"
#include "term.h"
#include "thread.h"
#include "video.h"

void __rtlib_setup(); // forward declaration

term term_console;
term term_log;
video screen;

thread_vtable _thread_vtable;
thread_vtable _program_thread_vtable;

raw_bitmap_vtable _raw_bitmap_vtable;
raw_bitmap_vtable _raw_bitmap_in_memory_vtable;
raw_bitmap_vtable _video_vtable;

static bitmap_word mouse_bitmap[MOUSE_WIDTH_IN_BITMAP_WORDS * MOUSE_HEIGHT * 4];

raw_bitmap_in_memory mouse_save;

//-----------------------------------------------------------------------------

#define KBRD_INTRFC 0x64
#define KBRD_RESET 0xFE /* reset CPU command */

void reboot() {
  term_write(cout, "ATT! Rebooting...\n");

  // Start with the official keyboard controller trick
  uint8 temp;
  do {
    temp = inb(0x64);
    if ((temp & 0x01) != 0) {
      (void)inb(0x60);
      continue;
    }
  } while ((temp & 0x02) != 0);

  /* Reset! */
  outb(0xFE, 0x64);

  debug_write("Did not reset with keyboard trick");

  // If at this point we did not reboot, try this little hack
  // Some magic reboot code found at
  // https://forum.osdev.org/viewtopic.php?f=1&t=18769
  outb(0x8F, 0x70);
  outb(0x00, 0x71);
  outb(0x00, 0x70);
  outb(inb(0x92) | 1, 0x92);

  // At this point the reboot procedure did not work and there is nothing to do
  term_write(cout, "Rebooting failed...\n");
}

void panic(unicode_string msg) {
  if (ARE_INTERRUPTS_ENABLED())
    disable_interrupts();

#ifdef RED_PANIC_SCREEN
  // The panic screen will take the top part of the screen to allow messges
  // in the console to be read.
  raw_bitmap_fill_rect((raw_bitmap *)&screen, 0, 0, 640, 120, &pattern_red);

  font_draw_string(&font_mono_6x13, &screen.super, 0, 0, msg, &pattern_white,
                   &pattern_black);
#else
  debug_write("PANIC!");
  unicode_char *p = msg;
  while (*p != '\0') {
    native_char c = *p & 0xFF;
    _debug_write(c);
    p++;
  }
#endif

  // Wait a bit...
  for (uint32 i = 0; i < CAST(uint32, (-1)); ++i) {
    NOP();
  }

  reboot();
  // ** NEVER REACHED ** (this function never returns)
}

//-----------------------------------------------------------------------------

// 64 bit arithmetic routines.

extern "C" uint64 __umoddi3(uint64 n, uint64 d) { // d must fit in 32 bits
  uint32 q0;
  uint32 q1;
  uint32 r;
  uint32 n0 = n;
  uint32 n1 = n >> 32;
  uint32 dv = d;

  __asm__("divl %4" : "=a"(q1), "=d"(r) : "0"(n1), "1"(0), "rm"(dv));
  n1 -= q1 * dv;
  __asm__("divl %4" : "=a"(q0), "=d"(r) : "0"(n0), "1"(n1), "rm"(dv));

  return r;
}

extern "C" uint64 __udivdi3(uint64 n, uint64 d) { // d must fit in 32 bits
  uint32 q0;
  uint32 q1;
  uint32 r;
  uint32 n0 = n;
  uint32 n1 = n >> 32;
  uint32 dv = d;

  __asm__("divl %4" : "=a"(q1), "=d"(r) : "0"(n1), "1"(0), "rm"(dv));
  n1 -= q1 * dv;
  __asm__("divl %4" : "=a"(q0), "=d"(r) : "0"(n0), "1"(n1), "rm"(dv));

  return (CAST(uint64, q1) << 32) + q0;
}

uint8 log2(uint32 n) {
  uint8 i = 0;

  while ((n >>= 1) != 0)
    i++;

  return i;
}

//-----------------------------------------------------------------------------

// Memory management functions.

void *kmalloc(size_t size) { return heap_malloc(&kheap, size); }

void kfree(void *ptr) { heap_free(&kheap, ptr); }

static void setup_kheap() {
  heap_init(&kheap, CAST(void *, 32 * (1 << 20) + GAMBIT_SHARED_MEM_LEN),
            32 * (1 << 20) - GAMBIT_SHARED_MEM_LEN);
}

static void setup_appheap(void *start, uint64 len) {
  heap_init(&appheap, CAST(void *, start), len);
}

extern "C" void *memcpy(void *dest, const void *src, size_t n) {
  uint8 *d = CAST(uint8 *, dest);
  uint8 *s = CAST(uint8 *, src);
  while (n-- > 0)
    *d++ = *s++;
  return dest;
}

native_string copy_without_trailing_spaces(uint8 *src, native_string dst,
                                           uint32 n) {
  uint32 i;
  uint32 end = 0;

  for (i = 0; i < n; i++) {
    dst[i] = src[i];
    if (src[i] != ' ')
      end = i + 1;
  }

  return dst + end;
}

uint32 kstrlen(native_string a) {
  native_char *p = a;
  while (*p != '\0') {
    p++;
  }
  return CAST(uint32, (p - a) + 1);
}

// Based off glibc's strcmp
// I roughly modified it to fit in the general code style of mimosa
int16 kstrcmp(native_string a, native_string b) {
  native_string s1 = CAST(native_string, a);
  native_string s2 = CAST(native_string, b);
  native_char c1, c2;
  do {
    c1 = CAST(native_char, *s1++);
    c2 = CAST(native_char, *s2++);
    if (c1 == '\0')
      return c1 - c2;
  } while (c1 == c2);
  return c1 - c2;
}

native_string kstrconcat(native_string a, native_string b) {
  uint32 alen = 0, blen = 0;
  native_char *p;
  native_string out;

  p = a;
  while (*p != '\0') {
    alen++;
    p++;
  }

  p = b;
  while (*p != '\0') {
    blen++;
    p++;
  }

  out = CAST(native_string, kmalloc(sizeof(native_char) * (alen + blen + 1)));
  memcpy(out, a, alen);
  memcpy(out + alen, b, blen);
  out[alen + blen] = '\0';

  return out;
}

//-----------------------------------------------------------------------------

// The global constructors and destructors are invoked by the
// functions "__do_global_ctors" and "__do_global_dtors".

typedef void (*func_ptr)();

extern func_ptr __CTOR_LIST__[];
extern func_ptr __DTOR_LIST__[];

void __do_global_ctors() {
  unsigned long nptrs = CAST(unsigned long, __CTOR_LIST__[0]);
  unsigned int i;
  if (nptrs == CAST(unsigned long, -1))
    for (nptrs = 0; __CTOR_LIST__[nptrs + 1] != 0; nptrs++)
      ;

  for (i = nptrs; i >= 1; i--)
    __CTOR_LIST__[i]();
  // Video VTables
  _video_vtable.hide_mouse = video_hide_mouse;
  _video_vtable.show_mouse = video_show_mouse;
  _video_vtable._select_layer = video_select_layer;

  _thread_vtable.thread_run = virtual_thread_run;
  _program_thread_vtable.thread_run = virtual_program_thread_run;

  _raw_bitmap_in_memory_vtable.hide_mouse = raw_bitmap_in_memory_hide_mouse;
  _raw_bitmap_in_memory_vtable.show_mouse = raw_bitmap_in_memory_show_mouse;
  _raw_bitmap_in_memory_vtable._select_layer =
      _raw_bitmap_in_memory_select_layer;

  _raw_bitmap_vtable.hide_mouse = raw_bitmap_hide_mouse;
  _raw_bitmap_vtable.show_mouse = raw_bitmap_show_mouse;
  _raw_bitmap_vtable._select_layer = _raw_bitmap_select_layer;

  // Screen
  video_init(&screen);

  raw_bitmap_fill_rect(&screen.super, 0, 0, screen.super._width,
                       screen.super._height, &pattern_gray50);

  raw_bitmap_in_memory_init(
      &mouse_save, mouse_bitmap,
      MOUSE_WIDTH_IN_BITMAP_WORDS << LOG2_BITMAP_WORD_WIDTH, MOUSE_HEIGHT, 4);

  // Create the console terminal

  term_init(&term_console, 0, 0, 80, 24, &font_mono_6x13, &font_mono_6x13B,
            L"console", TRUE);

  term_init(&term_log, 490, 0, 35, 76, &font_mono_4x6, &font_mono_4x6, L"log",
            TRUE);
}

void __do_global_dtors() {
  // Not implemented yet.
}

//-----------------------------------------------------------------------------

// The function "__rtlib_entry" is called by "kernel.s".  It is
// responsible for setting up the runtime library, and to execute the
// "main" function.

static void setup_bss() {
  extern uint8 edata[], end[];
  uint8 *p = edata;
  while (p < end) // zero out BSS
    *p++ = 0;
}

/**
 * Init a memory zone
 **/
void init(memory_zone *z) {
  /*
   * Cant access the lower addresses (like 0)
   * and cannot clean up the unusable zones.
   * All we really want to do is make sure Gambit runs
   * on reboot anyways
   */
  if (z->base > 0 && z->type == MEMORY_ZONE_USABLE) {
    uint8 *mem = CAST(uint8 *, z->base);
    uint64 l = z->length;
    for (uint64 i = 0; i < l; ++i) {
      mem[i] = 0;
    }
  }
}

extern "C" void __rtlib_entry() {
  setup_bss();

  uint16 no_of_entries = *CAST(uint16 *, MEMORY_ZONES_COUNT_START);
  memory_zone *zones = CAST(memory_zone *, MEMORY_ZONES_START);

#ifdef PRINT_MEMORY_LAYOUT
  debug_write("Memory layout:");
#endif
  uint16 index = 0;
  for (uint16 i = 0; i < no_of_entries; ++i) {
    memory_zone z = zones[i];
#ifdef PRINT_MEMORY_LAYOUT
    __debug_write(z.base);
    __debug_write("+");
    __debug_write(z.length);
    __debug_write(" : ");
#endif
    if (z.type == MEMORY_ZONE_USABLE) {
      if (MEMORY_ZONE_CONTAINS(z, END_KERNEL_HEAP)) {
        index = i;
      }

      init(&z);

#ifdef PRINT_MEMORY_LAYOUT
      debug_write("USABLE");
#endif
    } else {
#ifdef PRINT_MEMORY_LAYOUT
      debug_write("RESERVED");
#endif
    }
  }

  memory_zone heap_zone = zones[index];
  uint64 base = heap_zone.base;
  uint64 len = heap_zone.length;
  uint64 tot = base + len;

  void *app_heap_start = CAST(void *, END_KERNEL_HEAP);
  uint64 app_heap_len = (tot - END_KERNEL_HEAP);

  setup_kheap();
  setup_appheap(app_heap_start, app_heap_len);

  setup_intr();
  setup_time();

  __do_global_ctors();

  sched_setup(&__rtlib_setup);
  // ** NEVER REACHED ** (this function never returns)
}

//-----------------------------------------------------------------------------

static void identify_cpu() {
  uint32 max_fn;
  native_char vendor[13];
  uint32 processor, dummy, features;

  cpuid(0, max_fn, CAST(uint32 *, vendor)[0], CAST(uint32 *, vendor)[2],
        CAST(uint32 *, vendor)[1]);
  vendor[12] = '\0';
  cpuid(1, processor, dummy, dummy, features);

#ifdef SHOW_CPU_INFO

  term_write(cout, "CPU is ");
  term_write(cout, vendor);
  term_write(cout, "\n family=");
  term_write(cout, ((processor >> 8) & 0xf));
  term_write(cout, "\n model=");
  term_write(cout, ((processor >> 4) & 0xf));
  term_write(cout, "\n stepping=");
  term_write(cout, (processor & 0xf));
  term_write(cout, "\n has:\n");

  // For meaning of these values check:
  //   http://grafi.ii.pw.edu.pl/gbm/x86/cpuid.html

  if (features & HAS_FPU)
    term_write(cout, "  Floating Point Unit\n");
  if (features & HAS_VME)
    term_write(cout, "  V86 Mode Extensions\n");
  if (features & HAS_DE)
    term_write(cout, "  Debug Extensions\n");
  if (features & HAS_PSE)
    term_write(cout, "  Page Size Extensions\n");
  if (features & HAS_TSC)
    term_write(cout, "  Time Stamp Counter\n");
  if (features & HAS_MSR)
    term_write(cout, "  Model Specific Registers\n");
  if (features & HAS_PAE)
    term_write(cout, "  Physical Address Extensions\n");
  if (features & HAS_MCE)
    term_write(cout, "  Machine Check Exception\n");
  if (features & HAS_CX8)
    term_write(cout, "  CMPXCHG8B instruction\n");
  if (features & HAS_APIC)
    term_write(cout, "  Local APIC\n");
  if (features & HAS_SEP)
    term_write(cout, "  Fast system call\n");
  if (features & HAS_MTRR)
    term_write(cout, "  Memory Type Range Registers\n");
  if (features & HAS_PGE)
    term_write(cout, "  Page Global Enable\n");
  if (features & HAS_MCA)
    term_write(cout, "  Machine Check Architecture\n");
  if (features & HAS_CMOV)
    term_write(cout, "  Conditional MOVe\n");
  if (features & HAS_PAT)
    term_write(cout, "  Page Attribute Table\n");
  if (features & HAS_PSE36)
    term_write(cout, "  36 bit Page Size Extensions\n");
  if (features & HAS_PSN)
    term_write(cout, "  Processor Serial Number\n");
  if (features & HAS_CFLSH)
    term_write(cout, "  Cache Flush\n");
  if (features & HAS_DTES)
    term_write(cout, "  Debug Trace Store\n");
  if (features & HAS_ACPI)
    term_write(cout, "  ACPI support\n");
  if (features & HAS_MMX)
    term_write(cout, "  MultiMedia Extensions\n");
  if (features & HAS_FXSR)
    term_write(cout, "  FXSAVE and FXRSTOR\n");
  if (features & HAS_SSE)
    term_write(cout, "  SSE instructions\n");
  if (features & HAS_SSE2)
    term_write(cout, "  SSE2 instructions\n");
  if (features & HAS_SELFSNOOP)
    term_write(cout, "  Self Snoop\n");
  if (features & HAS_ACC)
    term_write(cout, "  Automatic clock control\n");
  if (features & HAS_IA64)
    term_write(cout, "  IA64 instructions\n");

#ifdef USE_TSC_FOR_TIME

  term_write(cout, "CPU clock = ");
  term_write(cout, _tsc_counts_per_sec);
  term_write(cout, " Hz\n");

#ifdef USE_APIC_FOR_TIMER

  term_write(cout, "CPU/bus clock multiplier = ");
  term_write(cout, _cpu_bus_multiplier.num);
  if (_cpu_bus_multiplier.den != 1) {
    term_write(cout, "/");
    term_write(cout, _cpu_bus_multiplier.den);
    if (_cpu_bus_multiplier.num < _cpu_bus_multiplier.den) {
      term_write(cout, "(strange that it is less than 1)");
    }
  }
  term_write(cout, "\n");

#endif
#endif
#endif
}

#ifdef GAMBIT_GSTATE

#include "modifiedgambit.h"

bool cut = 0;

void cut_ide_support() { cut = 1; }

bool has_cut_ide_support() { return cut; }

#define BU (NULL != ___local_gstate)

bool bridge_up() { return BU; }

uint32 flow_uncontrolled_writer = 0;
uint32 flow_controlled_writer = 0;

/**
 * Send a Gambit interrupt. If
 * the gambit bridge is not configured,
 * the function returns 0 and the interrupt
 * must be handled locally
 */
uint8 send_gambit_int(uint8 int_no, uint8 *params, uint8 len) {
  ASSERT_INTERRUPTS_DISABLED();

  if (BU) {
    /*
     * For more details in how the interrupt queue synchronizes itself with
     * the Scheme layer, @see the ./scheme/gambini.scm file.
     */

    uint8 *mem = NULL;
    uint32 *gambit_writer = NULL;
    uint32 max_len = 0;

    if (FLOW_CONTROLLED(int_no)) {
      mem = CAST(uint8 *, GAMBIT_FLOW_CONTROLLED_START);
      max_len = GAMBIT_FLOW_CONTROLLED_LEN;
      gambit_writer = &flow_controlled_writer;
    } else {
      mem = CAST(uint8 *, GAMBIT_FLOW_UNCONTROLLED_START);
      max_len = GAMBIT_FLOW_UNCONTROLLED_LEN;
      gambit_writer = &flow_uncontrolled_writer;
    }

    /**
     * Interrupts arguments are sent to gambit in the following way:
     *  |                   |              |
     *  |  INTERRUPT NUMBER | SZ OF PARAMS | PARAM 0 | PARAM 1 | ...
     *  |                   |              |
     *  This structure is called an interrupt frame. The interrupt number (and
     * thus the frame) never starts by zero, which is how we can detect that the
     * memory is accessible.
     */
    uint8 required_len = 2 + len; // number, size, + params

    // By convention, if the interrupt number is 0, we are ok
    uint32 scout = *gambit_writer;

    uint32 i;

    /* __debug_write("Int no: "); */
    /* debug_write(int_no); */
    /* debug_write("-----------------"); */
    /* for(i = 0; i < 512; ++i) { */
    /*     __debug_write(mem[i]); */
    /* } */
    /* debug_write("-----------------"); */

    for (i = 0; i < required_len; ++i, scout = (scout + 1) % max_len) {
      if (0 != mem[scout]) {
        break;
      }
    }

    if (i != required_len) {
      // This should be avoided at all cost
      /* debug_write("Interrupt queue full. Discarding"); */
    } else {
      scout = *gambit_writer;
      mem[scout % max_len] = int_no;
      mem[(scout + 1) % max_len] = len;

      for (uint8 i = 0; i < len; ++i) {
        mem[(scout + 2 + i) % max_len] = params[i];
      }

      *gambit_writer = (*gambit_writer + required_len) % max_len;
    }

    // Tell Gambit something is ready. This interrupt
    // might not be handled right away because of garbage
    // collections, and this is way we cache the interrupts on
    // the C side as well as the Scheme side.
    ___local_gstate->___raise_interrupt(GAMBIT_COMM_INT);
    return 1;
  } else {
    return 0;
  }
}

#else
#warning "Gambit interrupt handling does is not implemented"

uint8 send_gambit_int(uint8 int_no, uint8 *params, uint8 len) {
  panic(L"!TEMP!");
  return 0;
}

#endif

void idle_thread_run() {
#ifdef SHOW_HEARTBEAT
  uint8 heartbeat_cycle = 0;
  uint32 heartbeat_counter = 0;
#endif

  for (;;) {
#ifdef SHOW_HEARTBEAT

    if (0 == (heartbeat_counter % HEARTBEAT_FREQ)) {
      if (0 == heartbeat_cycle) {
        disable_interrupts();
        raw_bitmap_fill_rect((raw_bitmap *)&screen, 610, 0, 630, 20,
                             &pattern_red);
        heartbeat_cycle = 1;
        enable_interrupts();
      } else {
        disable_interrupts();
        raw_bitmap_fill_rect((raw_bitmap *)&screen, 610, 0, 630, 20,
                             &pattern_green);
        heartbeat_cycle = 0;
        enable_interrupts();
      }
    }
    heartbeat_counter = (heartbeat_counter + 1) % HEARTBEAT_FREQ;

#endif
    ASSERT_INTERRUPTS_ENABLED();
    thread_yield();
  }
}

extern void libc_init(void);

void __rtlib_setup() {
  error_code err;
  thread *the_idle = NULL;
  uint8 *cmd = NULL;
#ifdef USE_CACHE_BLOCK_MAID

  thread *cache_block_maid_thread;

#endif

  uint8 *mem = CAST(uint8 *, GAMBIT_SHARED_MEM_CMD);
  for (uint32 i = 0; i < GAMBIT_SHARED_MEM_LEN; ++i) {
    mem[i] = 0;
  }

  ASSERT_INTERRUPTS_ENABLED();

  term_write(cout, "Initializing ");
  term_write(cout, "\033[46m");
  term_write(cout, "Mimosa");
  term_write(cout, "\033[0m\n\n");

  identify_cpu();

  the_idle = CAST(thread *, kmalloc(sizeof(thread)));
  the_idle = new_thread(the_idle, idle_thread_run, "Idle thread");
  // the_idle->_prio = null_priority;
  the_idle->_quantum = frequency_to_time(10000); // Temp path for issue #56

  thread_start(the_idle);

  term_write(cout, "Loading up disks...\n");
  setup_disk();

  term_write(cout, "Loading up IDE controllers...\n");
  setup_ide();

  term_write(cout, "Loading up the virtual file system...\n");

  if (ERROR(err = init_vfs())) {
    goto setup_panic;
  }

  if (ERROR(err = setup_ps2()))
    goto setup_panic;

  if (ERROR(err = init_terms())) {
    goto setup_panic;
  }

  term_write(cout, "Cleaning up communication memory space\n");
  // Clean the memory
  cmd = CAST(uint8 *, GAMBIT_SHARED_MEM_CMD);

  for (uint32 i = 0; i < 512; ++i) {
    cmd[i] = 0;
  }

  /* Make a messenger thread to take care of communications from gambit */
  term_write(cout, "Loading up LIBC\n");
  libc_init();

  // FS is loaded, now load the cache maid
#ifdef USE_CACHE_BLOCK_MAID

  term_write(cout, "Loading the cache block maid...\n");
  cache_block_maid_thread = CAST(thread *, kmalloc(sizeof(thread)));
  thread_start(new_thread(cache_block_maid_thread, cache_block_maid_run,
                          "Cache block maid"));

#endif

  main();

  __do_global_dtors();
  panic(L"System termination");

setup_panic:
  debug_write("Panic");
  panic(L"Boot error");
}

//-----------------------------------------------------------------------------

// Local Variables: //
// mode: C++ //
// End: //
