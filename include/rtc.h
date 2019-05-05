// file: "rtc.h"

// Copyright (c) 2001 by Marc Feeley and Université de Montréal, All
// Rights Reserved.
//
// Revision History
// 22 Sep 01  initial version (Marc Feeley)

#ifndef __RTC_H
#define __RTC_H

//-----------------------------------------------------------------------------

#include "general.h"

//-----------------------------------------------------------------------------

//
// Definitions for MC 146818 real-time clock chip.
//

#define RTC_PORT_ADDR 0x70
#define RTC_PORT_DATA 0x71
#define RTC_IRQ 8

#define RTC_SEC          0
#define RTC_SEC_ALARM    1
#define RTC_MIN          2
#define RTC_MIN_ALARM    3
#define RTC_HOUR         4
#define RTC_HOUR_ALARM   5
#define RTC_DAY_IN_WEEK  6
#define RTC_DAY_IN_MONTH 7
#define RTC_MONTH        8
#define RTC_YEAR         9
#define RTC_REGA         10
#define RTC_REGB         11
#define RTC_REGC         12
#define RTC_REGD         13

#define RTC_REGA_UIP     (1<<7) // Update In Progress
#define RTC_REGA_OSC     (7<<4) // Oscillator Control
#define RTC_REGA_OSC_ON  (2<<4) // Oscillator On
#define RTC_REGA_OSC_OFF (0<<4) // Oscillator Off
#define RTC_REGA_8192HZ  3 // Periodic Interrupt Rate
#define RTC_REGA_4096HZ  4
#define RTC_REGA_2048HZ  5
#define RTC_REGA_1024HZ  6
#define RTC_REGA_512HZ   7
#define RTC_REGA_256HZ   8
#define RTC_REGA_128HZ   9
#define RTC_REGA_64HZ    10
#define RTC_REGA_32HZ    11
#define RTC_REGA_16HZ    12
#define RTC_REGA_8HZ     13
#define RTC_REGA_4HZ     14
#define RTC_REGA_2HZ     15

#define RTC_REGB_SET    (1<<7) // Disable Clock Updates
#define RTC_REGB_PIE    (1<<6) // Periodic Interrupt Enable
#define RTC_REGB_AIE    (1<<5) // Alarm Interrupt Enable
#define RTC_REGB_UIE    (1<<4) // Update Ended Interrupt Enable
#define RTC_REGB_SQWE   (1<<3) // Square Wave Enable
#define RTC_REGB_DM     (1<<2) // Data Mode mask
#define RTC_REGB_DM_BIN (1<<2) // Binary Data Mode
#define RTC_REGB_DM_BCD (0<<2) // Binary Coded Decimal Data Mode
#define RTC_REGB_2412   (1<<1) // 24/12 hour mode mask
#define RTC_REGB_24     (1<<1) // 24 hour mode
#define RTC_REGB_12     (0<<1) // 12 hour mode
#define RTC_REGB_DSE    (1<<0) // Daylight Savings Enable

#define RTC_REGC_IRQF   (1<<7) // Interrupt Request Flag
#define RTC_REGC_PF     (1<<6) // Periodic Interrupt Flag
#define RTC_REGC_AF     (1<<5) // Alarm Interrupt Flag
#define RTC_REGC_UF     (1<<4) // Update Ended Interrupt Flag

#define RTC_REGD_VRT    (1<<7) // Valid Ram and Time

//-----------------------------------------------------------------------------

#endif

// Local Variables: //
// mode: C++ //
// End: //
