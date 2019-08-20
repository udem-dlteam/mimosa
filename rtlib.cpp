// file: "rtlib.cpp"

// Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 22 Sep 01  initial version (Marc Feeley)

//-----------------------------------------------------------------------------

#include "drivers/filesystem/include/vfs.h"
#include "drivers/filesystem/include/stdstream.h"
#include "rtlib.h"
#include "heap.h"
#include "intr.h"
#include "chrono.h"
#include "ide.h"
#include "disk.h"
#include "ps2.h"
#include "term.h"
#include "thread.h"
#include "video.h"

void __rtlib_setup (); // forward declaration

term term_console;
video screen;

thread_vtable _thread_vtable;
thread_vtable _program_thread_vtable;

raw_bitmap_vtable _raw_bitmap_vtable;
raw_bitmap_vtable _raw_bitmap_in_memory_vtable;
raw_bitmap_vtable _video_vtable;

static bitmap_word mouse_bitmap[MOUSE_WIDTH_IN_BITMAP_WORDS * MOUSE_HEIGHT * 4];

raw_bitmap_in_memory mouse_save;

//-----------------------------------------------------------------------------

void panic(unicode_string msg) {
  __asm__ __volatile__("cli" : : : "memory");

#ifdef RED_PANIC_SCREEN
  raw_bitmap_fill_rect((raw_bitmap*)&screen, 0, 0, 640, 120, &pattern_red);

  font_draw_string(&font_mono_6x13, &screen.super, 0, 0, msg, &pattern_white,
                   &pattern_black);
#else
  debug_write("PANIC!");
  unicode_char* p = msg;
  while(*p != '\0') {
    native_char c = *p & 0xFF;
    _debug_write(c);
    p++;
  }
#endif

  for (;;) NOP();  // freeze execution
  // ** NEVER REACHED ** (this function never returns)
}

//-----------------------------------------------------------------------------

// 64 bit arithmetic routines.

extern "C"
uint64 __umoddi3 (uint64 n, uint64 d) // d must fit in 32 bits
{
  uint32 q0;
  uint32 q1;
  uint32 r;
  uint32 n0 = n;
  uint32 n1 = n >> 32;
  uint32 dv = d;

  __asm__ ("divl %4" : "=a" (q1), "=d" (r) : "0" (n1), "1" (0), "rm" (dv));
  n1 -= q1 * dv;
  __asm__ ("divl %4" : "=a" (q0), "=d" (r) : "0" (n0), "1" (n1), "rm" (dv));

  return r;
}

extern "C"
uint64 __udivdi3 (uint64 n, uint64 d) // d must fit in 32 bits
{
  uint32 q0;
  uint32 q1;
  uint32 r;
  uint32 n0 = n;
  uint32 n1 = n >> 32;
  uint32 dv = d;

  __asm__ ("divl %4" : "=a" (q1), "=d" (r) : "0" (n1), "1" (0), "rm" (dv));
  n1 -= q1 * dv;
  __asm__ ("divl %4" : "=a" (q0), "=d" (r) : "0" (n0), "1" (n1), "rm" (dv));

  return (CAST(uint64,q1) << 32) + q0;
}

uint8 log2 (uint32 n)
{
  uint8 i = 0;

  while ((n >>= 1) != 0)
    i++;

  return i;
}

//-----------------------------------------------------------------------------

// Memory management functions.

void* kmalloc(size_t size) {
  return heap_malloc(&kheap, size);
}

void kfree(void* ptr) {
  heap_free(&kheap, ptr);
}

static void setup_kheap() {
  heap_init(&kheap, CAST(void*,11*(1<<20)), 10*(1<<20));
}

static void setup_appheap() {
  heap_init(&appheap, CAST(void*, 32 * (1 << 20)), 256 * (1 << 20));
}

extern "C" void* memcpy (void* dest, const void* src, size_t n)
{
  uint8* d = CAST(uint8*,dest);
  uint8* s = CAST(uint8*,src);
  while (n-- > 0) *d++ = *s++;
  return dest;
}

native_string copy_without_trailing_spaces(uint8* src, native_string dst,
                                           uint32 n) {
  uint32 i;
  uint32 end = 0;

  for (i = 0; i < n; i++) {
    dst[i] = src[i];
    if (src[i] != ' ') end = i + 1;
  }

  return dst + end;
}

uint32 kstrlen(native_string a) {
  native_char* p = a;
  while(*p != '\0') {
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
  do
    {
      c1 = CAST(native_char, *s1++);
      c2 = CAST(native_char, *s2++);
      if (c1 == '\0')
        return c1 - c2;
    }
  while (c1 == c2);
  return c1 - c2;
}

native_string kstrconcat(native_string a, native_string b) {
  uint32 alen = 0, blen = 0;
  native_char *p;
  native_string out;

  p = a;
  while(*p != '\0') {
    alen++;
    p++;
  }

  p = b;
   while(*p != '\0') {
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

// The function "__pure_virtual" is called when a pure virtual method
// is invoked.

extern "C"
void __pure_virtual ()
{
  panic(L"pure virtual function called");
}

extern "C"
void __cxa_pure_virtual ()
{
  panic(L"pure virtual function called");
}

//-----------------------------------------------------------------------------

// The global constructors and destructors are invoked by the
// functions "__do_global_ctors" and "__do_global_dtors".

typedef void (*func_ptr) ();

extern func_ptr __CTOR_LIST__[];
extern func_ptr __DTOR_LIST__[];

void __do_global_ctors ()
{
  unsigned long nptrs = CAST(unsigned long,__CTOR_LIST__[0]);
  unsigned int i;
  if (nptrs == CAST(unsigned long,-1))
    for (nptrs = 0; __CTOR_LIST__[nptrs + 1] != 0; nptrs++);
  for (i = nptrs; i >= 1; i--)
    __CTOR_LIST__[i] ();
  // Video VTables
  _video_vtable.hide_mouse = video_hide_mouse;
  _video_vtable.show_mouse = video_show_mouse;
  _video_vtable._select_layer = video_select_layer;

  _thread_vtable.thread_run = virtual_thread_run;
  _program_thread_vtable.thread_run = virtual_program_thread_run;

  _raw_bitmap_in_memory_vtable.hide_mouse = raw_bitmap_in_memory_hide_mouse;
  _raw_bitmap_in_memory_vtable.show_mouse = raw_bitmap_in_memory_show_mouse;
  _raw_bitmap_in_memory_vtable._select_layer = _raw_bitmap_in_memory_select_layer;

  _raw_bitmap_vtable.hide_mouse = raw_bitmap_hide_mouse;
  _raw_bitmap_vtable.show_mouse = raw_bitmap_show_mouse;
  _raw_bitmap_vtable._select_layer = _raw_bitmap_select_layer;
  
  // Screen
  video_init(&screen);

  raw_bitmap_fill_rect(&screen.super, 0, 0, screen.super._width,
                       screen.super._height, &pattern_gray50);

  raw_bitmap_in_memory_init
    (&mouse_save,
     mouse_bitmap, MOUSE_WIDTH_IN_BITMAP_WORDS << LOG2_BITMAP_WORD_WIDTH,
     MOUSE_HEIGHT, 4);

  // Create the console terminal
  term_init(&term_console, 0, 0, 80, 25,
            &font_mono_6x13, &font_mono_6x13B,
            L"console", TRUE);
}

void __do_global_dtors ()
{
  // Not implemented yet.
}

//-----------------------------------------------------------------------------

// The function "__rtlib_entry" is called by "kernel.s".  It is
// responsible for setting up the runtime library, and to execute the
// "main" function.

static void setup_bss ()
{
  extern uint8 edata[], end[];

  uint8* p = edata;

  while (p < end) // zero out BSS
    *p++ = 0;
}

extern "C"
void __rtlib_entry ()
{
  setup_bss ();
  setup_kheap();
  setup_appheap();
  setup_intr ();
  setup_time ();

  __do_global_ctors ();

  sched_setup (&__rtlib_setup);  
  // ** NEVER REACHED ** (this function never returns)
}

//-----------------------------------------------------------------------------

static void identify_cpu ()
{
  uint32 max_fn;
  native_char vendor[13];
  uint32 processor, dummy, features;

  cpuid (0,
         max_fn,
         CAST(uint32*,vendor)[0],
         CAST(uint32*,vendor)[2],
         CAST(uint32*,vendor)[1]);

  vendor[12] = '\0';

  cpuid (1, processor, dummy, dummy, features);

#define SHOW_CPU_INFO
#ifdef SHOW_CPU_INFO

  term_write(cout, "CPU is ");
  term_write(cout, vendor);
  term_write(cout, " family=");
  term_write(cout, ((processor >> 8) & 0xf));
  term_write(cout, " model=");
  term_write(cout, ((processor >> 4) & 0xf));
  term_write(cout, " stepping=");
  term_write(cout, (processor & 0xf));
  term_write(cout, "\n");

  // For meaning of these values check:
  //   http://grafi.ii.pw.edu.pl/gbm/x86/cpuid.html

  if (features & HAS_FPU)       term_write(cout,"  has Floating Point Unit\n");
  if (features & HAS_VME)       term_write(cout,"  has V86 Mode Extensions\n");
  if (features & HAS_DE)        term_write(cout,"  has Debug Extensions\n");
  if (features & HAS_PSE)       term_write(cout,"  has Page Size Extensions\n");
  if (features & HAS_TSC)       term_write(cout,"  has Time Stamp Counter\n");
  if (features & HAS_MSR)       term_write(cout,"  has Model Specific Registers\n");
  if (features & HAS_PAE)       term_write(cout,"  has Physical Address Extensions\n");
  if (features & HAS_MCE)       term_write(cout,"  has Machine Check Exception\n");
  if (features & HAS_CX8)       term_write(cout,"  has CMPXCHG8B instruction\n");
  if (features & HAS_APIC)      term_write(cout,"  has Local APIC\n");
  if (features & HAS_SEP)       term_write(cout,"  has Fast system call\n");
  if (features & HAS_MTRR)      term_write(cout,"  has Memory Type Range Registers\n");
  if (features & HAS_PGE)       term_write(cout,"  has Page Global Enable\n");
  if (features & HAS_MCA)       term_write(cout,"  has Machine Check Architecture\n");
  if (features & HAS_CMOV)      term_write(cout,"  has Conditional MOVe\n");
  if (features & HAS_PAT)       term_write(cout,"  has Page Attribute Table\n");
  if (features & HAS_PSE36)     term_write(cout,"  has 36 bit Page Size Extensions\n");
  if (features & HAS_PSN)       term_write(cout,"  has Processor Serial Number\n");
  if (features & HAS_CFLSH)     term_write(cout,"  has Cache Flush\n");
  if (features & HAS_DTES)      term_write(cout,"  has Debug Trace Store\n");
  if (features & HAS_ACPI)      term_write(cout,"  has ACPI support\n");
  if (features & HAS_MMX)       term_write(cout,"  has MultiMedia Extensions\n");
  if (features & HAS_FXSR)      term_write(cout,"  has FXSAVE and FXRSTOR\n");
  if (features & HAS_SSE)       term_write(cout,"  has SSE instructions\n");
  if (features & HAS_SSE2)      term_write(cout,"  has SSE2 instructions\n");
  if (features & HAS_SELFSNOOP) term_write(cout,"  has Self Snoop\n");
  if (features & HAS_ACC)       term_write(cout,"  has Automatic clock control\n");
  if (features & HAS_IA64)      term_write(cout,"  has IA64 instructions\n");

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

void idle_thread_run() {
  for(;;) {
    thread_yield();
  }
}

extern void libc_init(void);

void __rtlib_setup ()
{ 
  error_code err;
  thread* the_idle, *cache_block_maid_thread;

  ASSERT_INTERRUPTS_ENABLED();

  term_write(cout, "Initializing ");
  term_write(cout, "\033[46m");
  term_write(cout, "Mimosa");
  term_write(cout, "\033[0m\n\n");

  identify_cpu();

  the_idle = CAST(thread*, kmalloc(sizeof(thread)));
  the_idle = new_thread(the_idle, idle_thread_run, "Idle thread");
  // the_idle->_prio = null_priority;
  the_idle->_quantum = nanoseconds_to_time(1000); // Temp path for issue #56
  thread_start(the_idle);

  term_write(cout, "Loading up disks...\n");
  setup_disk();
  term_write(cout, "Loading up IDE controllers...\n");
  setup_ide ();
  term_write(cout, "Loading up the virtual file system...\n");

  if(ERROR(err = init_vfs())) {
    goto setup_panic;
  }

  if(ERROR(err = setup_ps2()))
    goto setup_panic;
  
  if(ERROR(err = init_terms())) {
    goto setup_panic;
  }
  
  term_write(cout, "Loading up LIBC\n");
  libc_init();

  // FS is loaded, now load the cache maid
#ifdef USE_CACHE_BLOCK_MAID
  term_write(cout, "Loading the cache block maid...\n");

  cache_block_maid_thread = CAST(thread*, kmalloc(sizeof(thread)));
  thread_start(new_thread(cache_block_maid_thread, cache_block_maid_run,
                          "Cache block maid"));
#endif

  main ();

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
