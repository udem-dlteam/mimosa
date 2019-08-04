// file: "apic.h"

// Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 23 Oct 01  initial version (Marc Feeley)

#ifndef __APIC_H
#define __APIC_H

//-----------------------------------------------------------------------------

#include "general.h"

//-----------------------------------------------------------------------------

//
// Definitions for local APIC (advanced programmable interrupt controller).
//

// Check: http://expert.cc.purdue.edu/~titzer/redpants/mpx86.html

#define MSR_APIC 27

#define MSR_APIC_BASE 0xfee00000
#define MSR_APIC_E    (1<<11)
#define MSR_APIC_BSP  (1<<8)

#define APIC_REG(n) *CAST(volatile uint32*,MSR_APIC_BASE+(n))

#define APIC_LOCAL_APIC_ID       APIC_REG (0x0020)
#define APIC_LOCAL_APIC_VERSION  APIC_REG (0x0030)
#define APIC_TPR                 APIC_REG (0x0080)
#define APIC_APR                 APIC_REG (0x0090)
#define APIC_PPR                 APIC_REG (0x00a0)
#define APIC_EOI                 APIC_REG (0x00b0)
#define APIC_LDR                 APIC_REG (0x00d0)
#define APIC_DFR                 APIC_REG (0x00e0)
#define APIC_SVR                 APIC_REG (0x00f0)
#define APIC_ISR                 APIC_REG (0x0100)
#define APIC_TMR                 APIC_REG (0x0180)
#define APIC_IRR                 APIC_REG (0x0200)
#define APIC_ESR                 APIC_REG (0x0280)
#define APIC_ICR1                APIC_REG (0x0300)
#define APIC_ICR2                APIC_REG (0x0310)
#define APIC_LVTT                APIC_REG (0x0320)
#define APIC_LVTTM               APIC_REG (0x0330)
#define APIC_LVTPC               APIC_REG (0x0340)
#define APIC_LVT0                APIC_REG (0x0350)
#define APIC_LVT1                APIC_REG (0x0360)
#define APIC_LVTE                APIC_REG (0x0370)
#define APIC_INITIAL_TIMER_COUNT APIC_REG (0x0380)
#define APIC_CURRENT_TIMER_COUNT APIC_REG (0x0390)
#define APIC_TIMER_DIVIDE_CONFIG APIC_REG (0x03e0)

#define APIC_EXT_INT (7<<8)
#define APIC_POLARITY0 (0<<13)
#define APIC_POLARITY1 (1<<13)

#define APIC_SVR_FPC_DISABLE (1<<9)
#define APIC_SVR_SW_ENABLE   (1<<8)
#define APIC_SVR_VECTOR_MASK 0xff

#define APIC_TPR_PRIO_MASK 0xff

#define APIC_LDR_LOGID_MASK (0xff << 24)

#define APIC_DFR_CONFIG(n) (((n) << 28) | 0xfffffff)

#define APIC_LVT_MASKED      (1<<16)
#define APIC_LVT_LTM         (1<<15)
#define APIC_LVT_RIRR        (1<<14)
#define APIC_LVT_POL         (1<<13)
#define APIC_LVT_DM_EXTINT   (7<<8)
#define APIC_LVT_DM_MASK     (7<<8)
#define APIC_LVT_VECTOR_MASK 0xff

#define APIC_LVTT_PERIODIC (1<<17)

//-----------------------------------------------------------------------------

#endif

// Local Variables: //
// mode: C     //
// End: //
