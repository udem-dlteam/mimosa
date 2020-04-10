// file: "chrono.cpp"

// Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 22 Sep 01  initial version (Marc Feeley)

//-----------------------------------------------------------------------------

#include "apic.h"
#include "asm.h"
#include "chrono.h"
#include "intr.h"
#include "rtc.h"
#include "rtlib.h"
#include "term.h"
#include "thread.h"

//-----------------------------------------------------------------------------

// Rational arithmetic routines.
#ifdef USE_TSC_FOR_TIME
#ifdef USE_APIC_FOR_TIMER

static rational make_rational(uint64 num, uint64 den) {
  rational result;
  uint64 x = num;
  uint64 y = den;

  while (y != 0) {
    int64 t = x;
    x = y;
    y = t % y;
  }

  result.num = num / x;
  result.den = den / x;

  return result;
}

static rational rational_floor(rational x) {
  rational result;

  result.num = x.num / x.den;
  result.den = 1;

  return result;
}

static rational rational_inverse(rational x) {
  return make_rational(x.den, x.num);
}

static bool rational_less_than(rational x, rational y) {
  return CAST(uint64, x.num) * y.den < CAST(uint64, y.num) * x.den;
}

static rational rational_add(rational x, rational y) {
  return make_rational(CAST(uint64, x.num) * y.den +
                           CAST(uint64, y.num) * x.den,
                       CAST(uint64, x.den) * y.den);
}

static rational rational_subtract(rational x, rational y) {
  return make_rational(CAST(uint64, x.num) * y.den -
                           CAST(uint64, y.num) * x.den,
                       CAST(uint64, x.den) * y.den);
}

static rational rational_rationalize2(rational x, rational y) {
  rational fx = rational_floor(x);
  rational fy = rational_floor(y);

  if (!rational_less_than(fx, x))
    return fx;

  if (rational_less_than(fx, fy) || rational_less_than(fy, fx))
    return rational_add(fx, make_rational(1, 1));

  return rational_add(fx, rational_inverse(rational_rationalize2(
                              rational_inverse(rational_subtract(y, fy)),
                              rational_inverse(rational_subtract(x, fx)))));
}

static rational rational_rationalize(rational x, rational y) {
  if (rational_less_than(y, x)) {
    rational t;
    t = y;
    y = x;
    x = t;
  }

  rational diff = rational_subtract(y, x);
  rational sum = rational_add(y, x);

  if (rational_less_than(diff, sum)) {
    return rational_rationalize2(diff, sum);
  }

  return diff;
}

#endif
#endif

//-----------------------------------------------------------------------------

#define EPOCH 1970

#define DAYS_SINCE_JAN_1_2000(year)                                            \
  (((year)-1) * 365 + ((year)-1) / 4 - ((year)-1) / 100 + ((year)-1) / 400 -   \
   730119)

static uint16 days_since_jan1[12] = {0,   31,  59,  90,  120, 151,
                                     181, 212, 243, 273, 304, 334};

#ifdef USE_IRQ8_FOR_TIME

volatile uint64 _irq8_counter = 0;
time pos_infinity = {18446744073709551615ULL};
time neg_infinity = {0};

extern "C" void irq8() {
  ACKNOWLEDGE_IRQ(8);

  _irq8_counter++;

  outb(RTC_REGC, RTC_PORT_ADDR); // must also read register C to
  inb(RTC_PORT_DATA);            // acknowledge RTC interrupt
}

#endif

#ifdef USE_TSC_FOR_TIME

static uint64 tsc_at_refpoint = 0;
uint32 _tsc_counts_per_sec = 0; // NOTE: works up to a 4.2 GHz processor clock
time pos_infinity = {18446744073709551615ULL};
time neg_infinity = {0};

#ifdef USE_APIC_FOR_TIMER

rational _cpu_bus_multiplier;

#endif

#endif

static int32 secs_since_epoch_at_refpoint = 0;

uint8 bcd_to_int(uint8 bcd) {
  // Convert "binary coded decimal" to integer.
  return 10 * (bcd >> 4) + (bcd & 15);
}

void setup_time() {
  // Interrupts needs to be disabled to
  // prevents accuracy issues
  ASSERT_INTERRUPTS_DISABLED();

#ifdef USE_IRQ8_FOR_TIME

#if IRQ8_COUNTS_PER_SEC == 2
#define RTC_RATE RTC_REGA_2HZ
#endif
#if IRQ8_COUNTS_PER_SEC == 4
#define RTC_RATE RTC_REGA_4HZ
#endif
#if IRQ8_COUNTS_PER_SEC == 8
#define RTC_RATE RTC_REGA_8HZ
#endif
#if IRQ8_COUNTS_PER_SEC == 16
#define RTC_RATE RTC_REGA_16HZ
#endif
#if IRQ8_COUNTS_PER_SEC == 32
#define RTC_RATE RTC_REGA_32HZ
#endif
#if IRQ8_COUNTS_PER_SEC == 64
#define RTC_RATE RTC_REGA_64HZ
#endif
#if IRQ8_COUNTS_PER_SEC == 128
#define RTC_RATE RTC_REGA_128HZ
#endif
#if IRQ8_COUNTS_PER_SEC == 256
#define RTC_RATE RTC_REGA_256HZ
#endif
#if IRQ8_COUNTS_PER_SEC == 512
#define RTC_RATE RTC_REGA_512HZ
#endif
#if IRQ8_COUNTS_PER_SEC == 1024
#define RTC_RATE RTC_REGA_1024HZ
#endif
#if IRQ8_COUNTS_PER_SEC == 2048
#define RTC_RATE RTC_REGA_2048HZ
#endif
#if IRQ8_COUNTS_PER_SEC == 4096
#define RTC_RATE RTC_REGA_4096HZ
#endif
#if IRQ8_COUNTS_PER_SEC == 8192
#define RTC_RATE RTC_REGA_8192HZ
#endif

  outb(RTC_REGA, RTC_PORT_ADDR);
  outb(RTC_REGA_OSC_ON | RTC_RATE, RTC_PORT_DATA);

  outb(RTC_REGB, RTC_PORT_ADDR);
  outb(RTC_REGB_PIE | RTC_REGB_DM_BCD | RTC_REGB_24, RTC_PORT_DATA);

#endif

#ifdef USE_IRQ8_FOR_TIME

  int samples_left = 2;

#endif

#ifdef USE_TSC_FOR_TIME

  int samples_left = 3;
  uint64 old_tsc = 0;

#ifdef USE_APIC_FOR_TIMER

  uint32 old_apic_timer_count = 0;

  APIC_INITIAL_TIMER_COUNT = 0xffffffff;

#endif
#endif

  uint8 old_sec = 255;

  for (;;) {
    outb(RTC_REGA, RTC_PORT_ADDR);
    if ((inb(RTC_PORT_DATA) & RTC_REGA_UIP) == 0) {
      outb(RTC_SEC, RTC_PORT_ADDR);
      uint8 new_sec = inb(RTC_PORT_DATA);

      if (old_sec != new_sec) {
#ifdef USE_TSC_FOR_TIME
        uint64 new_tsc = rdtsc();
#ifdef USE_APIC_FOR_TIMER
        uint32 new_apic_timer_count = APIC_CURRENT_TIMER_COUNT;
#endif
#endif

        if (--samples_left == 0) {
#ifdef USE_TSC_FOR_TIME

          tsc_at_refpoint = new_tsc;
          _tsc_counts_per_sec = new_tsc - old_tsc;

#ifdef USE_APIC_FOR_TIMER

          _cpu_bus_multiplier = rational_rationalize(
              make_rational(_tsc_counts_per_sec >> 10,
                            (old_apic_timer_count - new_apic_timer_count) >>
                                10),
              make_rational(1, 16));
#endif
#endif
          break;
        }

        old_sec = new_sec;

#ifdef USE_TSC_FOR_TIME

        old_tsc = new_tsc;

#ifdef USE_APIC_FOR_TIMER

        old_apic_timer_count = new_apic_timer_count;

#endif
#endif
      }
    }
  }

  uint8 sec, min, hour, day_in_month, month, year_in_century;
  int32 year;
  int16 day_in_year;

  outb(RTC_SEC, RTC_PORT_ADDR);
  sec = bcd_to_int(inb(RTC_PORT_DATA));

  outb(RTC_MIN, RTC_PORT_ADDR);
  min = bcd_to_int(inb(RTC_PORT_DATA));

  outb(RTC_HOUR, RTC_PORT_ADDR);
  hour = bcd_to_int(inb(RTC_PORT_DATA));

  outb(RTC_DAY_IN_MONTH, RTC_PORT_ADDR);
  day_in_month = bcd_to_int(inb(RTC_PORT_DATA));

  outb(RTC_MONTH, RTC_PORT_ADDR);
  month = bcd_to_int(inb(RTC_PORT_DATA));

  outb(RTC_YEAR, RTC_PORT_ADDR);
  year_in_century = bcd_to_int(inb(RTC_PORT_DATA));

  year = ((year_in_century <= 50) ? 2000 : 1900) + year_in_century;

  day_in_year = days_since_jan1[month - 1] + day_in_month;

  if (month > 2) { // after february count back from january 1st of next year
    year++;
    day_in_year -= 365;
  }

  // The following computes the number of seconds since the Epoch.
  // Note that leap seconds are ignored.

  secs_since_epoch_at_refpoint =
      (((DAYS_SINCE_JAN_1_2000(year) - DAYS_SINCE_JAN_1_2000(EPOCH) +
         day_in_year) *
            24 +
        hour) *
           60 +
       min) *
          60 +
      sec;

#ifdef USE_IRQ8_FOR_TIME

  _irq8_counter = 0;
  ENABLE_IRQ(8);

#endif
}

int gettimeofday(struct timeval *tv, struct timezone *tz) {
  if (tv != NULL) {
#ifdef USE_IRQ8_FOR_TIME

    /* disable_interrupts(); */
    uint64 n = _irq8_counter;
    /* enable_interrupts(); */

    tv->tv_sec = secs_since_epoch_at_refpoint + (n / IRQ8_COUNTS_PER_SEC);
    tv->tv_usec =
        (n % IRQ8_COUNTS_PER_SEC) * (1000000 / 2) / (IRQ8_COUNTS_PER_SEC / 2);

#endif

#ifdef USE_TSC_FOR_TIME

    uint64 cycles = rdtsc() - tsc_at_refpoint;
    uint64 secs = cycles / _tsc_counts_per_sec;

    cycles -= secs * _tsc_counts_per_sec;

    tv->tv_sec = secs_since_epoch_at_refpoint + secs;
    tv->tv_usec = cycles * 1000000 / _tsc_counts_per_sec;

#endif
  }

  if (tz != NULL) {
    tz->tz_minuteswest = 0;
    tz->tz_dsttime = 0;
  }

  return 0;
}

void get_current_time(uint8 *hour, uint8 *min, uint8 *sec) {
  outb(RTC_SEC, RTC_PORT_ADDR);
  *sec = bcd_to_int(inb(RTC_PORT_DATA));

  outb(RTC_MIN, RTC_PORT_ADDR);
  *min = bcd_to_int(inb(RTC_PORT_DATA));

  outb(RTC_HOUR, RTC_PORT_ADDR);
  *hour = bcd_to_int(inb(RTC_PORT_DATA));
}

void get_current_date(int16 *year, uint8 *month, uint8 *day) {
  uint8 year_in_century;

  outb(RTC_DAY_IN_MONTH, RTC_PORT_ADDR);
  *day = bcd_to_int(inb(RTC_PORT_DATA));

  outb(RTC_MONTH, RTC_PORT_ADDR);
  *month = bcd_to_int(inb(RTC_PORT_DATA));

  outb(RTC_YEAR, RTC_PORT_ADDR);
  year_in_century = bcd_to_int(inb(RTC_PORT_DATA));

  *year = ((year_in_century <= 50) ? 2000 : 1900) + year_in_century;
}

uint32 days_from_civil(int16 y, uint16 m, uint16 d) {
  y -= m <= 2;
  uint32 era = (y >= 0 ? y : y - 399) / 400;
  uint16 yoe = static_cast<unsigned>(y - era * 400);           // [0, 399]
  uint16 doy = (153 * (m + (m > 2 ? -3 : 9)) + 2) / 5 + d - 1; // [0, 365]
  uint32 doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;          // [0, 146096]
  return era * 146097 + doe - 719468;
}

// Local Variables: //
// mode: C++ //
// End: //
