// file: "chrono.h"

// Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 22 Sep 01  initial version (Marc Feeley)

#ifndef TIME_H
#define TIME_H

//-----------------------------------------------------------------------------

#include "general.h"
#include "asm.h"
#include "pit.h"

//-----------------------------------------------------------------------------

typedef struct { uint32 num, den; } rational;

//-----------------------------------------------------------------------------

struct timeval
  {
    uint32 tv_sec;   // Seconds
    uint32 tv_usec;  // Microseconds
  };

struct timezone
  {
    int32 tz_minuteswest;  // Minutes west of GMT
    int32 tz_dsttime;      // Nonzero if DST is ever in effect
  };

// Initialization of time manager.

void setup_time ();

// The function "gettimeofday" retrieves the time elapsed since the
// ``Epoch'' (00:00:00 on the 1st of january 1970, UTC).

int gettimeofday (struct timeval *tv, struct timezone *tz);

// High resolution time datatype.

#ifdef USE_IRQ8_FOR_TIME

typedef struct time { uint64 n; } time;

#define current_time() \
({ \
   time val; \
   disable_interrupts (); \
   val.n = _irq8_counter; \
   enable_interrupts (); \
   val; \
})

#define current_time_no_interlock() \
({ \
   time val; \
   val.n = _irq8_counter; \
   val; \
})

#define seconds_to_time(x) \
({ \
   time val; \
   val.n = CAST(uint64,x)*IRQ8_COUNTS_PER_SEC; \
   val; \
})

#define nanoseconds_to_time(x) \
({ \
   time val; \
   val.n = CAST(uint64,x)*IRQ8_COUNTS_PER_SEC/1000000000; \
   val; \
})

#define frequency_to_time(x) \
({ \
   time val; \
   val.n = IRQ8_COUNTS_PER_SEC / (x); \
   val; \
})

#define time_to_pit_counts(x) \
((x).n * PIT_COUNTS_PER_SEC / IRQ8_COUNTS_PER_SEC)

#define time_to_apic_timer_counts(x) \
((x).n * _tsc_counts_per_sec / (IRQ8_COUNTS_PER_SEC*_cpu_bus_multiplier.num))

#define add_time(x,y) \
({ \
   time val; \
   val.n = (x).n + (y).n; \
   val; \
})

#define subtract_time(x,y) \
({ \
   time val; \
   val.n = (x).n - (y).n; \
   val; \
})

#define equal_time(x,y) ((x).n == (y).n)
#define less_time(x,y) ((x).n < (y).n)

extern volatile uint64 _irq8_counter;
extern time pos_infinity;
extern time neg_infinity;

#endif

#ifdef USE_TSC_FOR_TIME

typedef struct time { uint64 n; } time;

#define current_time() \
({ \
   time val; \
   val.n = rdtsc (); \
   val; \
})

#define current_time_no_interlock() \
({ \
   time val; \
   val.n = rdtsc (); \
   val; \
})

#define seconds_to_time(x) \
({ \
   time val; \
   val.n = CAST(uint64,x)*_tsc_counts_per_sec; \
   val; \
})

#define nanoseconds_to_time(x) \
({ \
   time val; \
   val.n = CAST(uint64,x)*_tsc_counts_per_sec/1000000000; \
   val; \
})

#define frequency_to_time(x) \
({ \
   time val; \
   val.n = _tsc_counts_per_sec / (x); \
   val; \
})

#define time_to_pit_counts(x) \
((x).n * PIT_COUNTS_PER_SEC / _tsc_counts_per_sec)

#define time_to_apic_timer_counts(x) ((x).n/_cpu_bus_multiplier.num)

#define add_time(x,y) \
({ \
   time val; \
   val.n = (x).n + (y).n; \
   val; \
})

#define subtract_time(x,y) \
({ \
   time val; \
   val.n = (x).n - (y).n; \
   val; \
})

#define equal_time(x,y) ((x).n == (y).n)
#define less_time(x,y) ((x).n < (y).n)

extern uint32 _tsc_counts_per_sec;
extern time pos_infinity;
extern time neg_infinity;

#ifdef USE_APIC_FOR_TIMER
extern rational _cpu_bus_multiplier;
#endif

#endif

//-----------------------------------------------------------------------------

#endif

// Local Variables: //
// mode: C++ //
// End: //
