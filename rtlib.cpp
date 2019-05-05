// file: "rtlib.cpp"

// Copyright (c) 2001 by Marc Feeley and Université de Montréal, All
// Rights Reserved.
//
// Revision History
// 22 Sep 01  initial version (Marc Feeley)

//-----------------------------------------------------------------------------

#include "rtlib.h"
#include "intr.h"
#include "time.h"
#include "ide.h"
#include "disk.h"
#include "fs.h"
#include "ps2.h"
#include "term.h"
#include "thread.h"

static void __rtlib_setup (); // forward declaration

//-----------------------------------------------------------------------------

void fatal_error (native_string msg)
{
  disable_interrupts ();

  while (*msg != '\0')
    outb (*msg++, 0xe9); // under "bochs" this sends the message to the console

  for (;;) ; // freeze execution

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

// For now, a simple linear allocator is used.  Memory is never reclaimed.

static uint32 alloc_ptr = (1<<20); // start at 1MB

void* kmalloc (size_t size)
{
  uint32 ptr = alloc_ptr;

  alloc_ptr = ptr + ((size + 7) & ~7);

  return CAST(void*,ptr);
}

void kfree (void* ptr)
{
  // Not implemented yet.
}

// Implementation of the C++ "new" operator.

#if 0
void* __builtin_new (size_t size)
#else
void* operator new (size_t size)
#endif
{
  return kmalloc (size);
}

// Implementation of the C++ "delete" operator.

#if 0
void __builtin_delete (void* obj)
#else
void operator delete (void* obj)
#endif
{
  if (obj != NULL)
    kfree (obj);
}

// Implementation of the C++ "new[]" operator.

#if 0
void* __builtin_vec_new (size_t size)
#else
void* operator new[] (size_t size)
#endif
{
  return operator new (size);
}

// Implementation of the C++ "delete[]" operator.

#if 0
void __builtin_vec_delete (void* obj)
#else
void operator delete[] (void* obj)
#endif
{
  operator delete (obj);
}

extern "C"
void* memcpy (void* dest, const void* src, size_t n)
{
  uint8* d = CAST(uint8*,dest);
  uint8* s = CAST(uint8*,src);
  while (n-- > 0) *d++ = *s++;
  return dest;
}

//-----------------------------------------------------------------------------

// The function "__pure_virtual" is called when a pure virtual method
// is invoked.

extern "C"
void __pure_virtual ()
{
  fatal_error ("pure virtual function called");
}

extern "C"
void __cxa_pure_virtual ()
{
  fatal_error ("pure virtual function called");
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
  setup_intr ();
  setup_time ();

  __do_global_ctors ();

  scheduler::setup (&__rtlib_setup);

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

  cout << "CPU is " << vendor
       << " family=" << ((processor >> 8) & 0xf)
       << " model=" << ((processor >> 4) & 0xf)
       << " stepping=" << (processor & 0xf) << "\n";

  // For meaning of these values check:
  //   http://grafi.ii.pw.edu.pl/gbm/x86/cpuid.html

  if (features & HAS_FPU)       cout << "  has Floating Point Unit\n";
  if (features & HAS_VME)       cout << "  has V86 Mode Extensions\n";
  if (features & HAS_DE)        cout << "  has Debug Extensions\n";
  if (features & HAS_PSE)       cout << "  has Page Size Extensions\n";
  if (features & HAS_TSC)       cout << "  has Time Stamp Counter\n";
  if (features & HAS_MSR)       cout << "  has Model Specific Registers\n";
  if (features & HAS_PAE)       cout << "  has Physical Address Extensions\n";
  if (features & HAS_MCE)       cout << "  has Machine Check Exception\n";
  if (features & HAS_CX8)       cout << "  has CMPXCHG8B instruction\n";
  if (features & HAS_APIC)      cout << "  has Local APIC\n";
  if (features & HAS_SEP)       cout << "  has Fast system call\n";
  if (features & HAS_MTRR)      cout << "  has Memory Type Range Registers\n";
  if (features & HAS_PGE)       cout << "  has Page Global Enable\n";
  if (features & HAS_MCA)       cout << "  has Machine Check Architecture\n";
  if (features & HAS_CMOV)      cout << "  has Conditional MOVe\n";
  if (features & HAS_PAT)       cout << "  has Page Attribute Table\n";
  if (features & HAS_PSE36)     cout << "  has 36 bit Page Size Extensions\n";
  if (features & HAS_PSN)       cout << "  has Processor Serial Number\n";
  if (features & HAS_CFLSH)     cout << "  has Cache Flush\n";
  if (features & HAS_DTES)      cout << "  has Debug Trace Store\n";
  if (features & HAS_ACPI)      cout << "  has ACPI support\n";
  if (features & HAS_MMX)       cout << "  has MultiMedia Extensions\n";
  if (features & HAS_FXSR)      cout << "  has FXSAVE and FXRSTOR\n";
  if (features & HAS_SSE)       cout << "  has SSE instructions\n";
  if (features & HAS_SSE2)      cout << "  has SSE2 instructions\n";
  if (features & HAS_SELFSNOOP) cout << "  has Self Snoop\n";
  if (features & HAS_ACC)       cout << "  has Automatic clock control\n";
  if (features & HAS_IA64)      cout << "  has IA64 instructions\n";

#ifdef USE_TSC_FOR_TIME

  cout << "CPU clock = " << _tsc_counts_per_sec << " Hz\n";

#ifdef USE_APIC_FOR_TIMER

  cout << "CPU/bus clock multiplier = " << _cpu_bus_multiplier.num;

  if (_cpu_bus_multiplier.den == 1)
    cout << "\n";
  else
    {
      cout << "/" << _cpu_bus_multiplier.den << " is not an integer!\n";
      fatal_error ("CPU/bus clock multiplier is not an integer\n");
    }

#endif
#endif
#endif
}

class idle_thread : public thread
  {
  public:

    idle_thread ();

  protected:

    virtual void run ();
  };

idle_thread::idle_thread ()
{
}

void idle_thread::run ()
{
  for (;;)
    thread::yield ();
}

static void __rtlib_setup ()
{
  cout << "Initializing " << "\033[46m" << OS_NAME << "\033[0m\n\n";

  identify_cpu ();

  //setup_ps2 ();

  //enable_interrupts ();

  //  (new idle_thread)->start (); // need an idle thread to prevent deadlocks

  //setup_disk ();
  //setup_ide ();
  //setup_fs ();
  //setup_net ();

  main ();

  __do_global_dtors ();

  fatal_error ("system termination");
}

//-----------------------------------------------------------------------------

// Local Variables: //
// mode: C++ //
// End: //
