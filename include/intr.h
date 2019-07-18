// file: "intr.h"

// Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 22 Sep 01  initial version (Marc Feeley)

#ifndef INTR_H
#define INTR_H

//-----------------------------------------------------------------------------

#include "general.h"
#include "asm.h"
#include "pic.h"
#include "apic.h"

//-----------------------------------------------------------------------------

typedef struct interrupt_data {
  uint32 ds;
  uint32 edi, esi, ebp, esp, ebx, edx, ecx, eax;
  uint32 int_no, error_code;
  uint32 eip, cs, eflags, useresp, ss;
} interrupt_data;


// Initialization of interrupt manager.

void setup_intr ();

// Enabling, disabling and acknowledging IRQs.

#define ENABLE_IRQ(n) \
do { \
     if ((n) < 8) \
       outb (inb (PIC_PORT_MASTER_IMR) & ~PIC_OCW1_MASK(n), \
             PIC_PORT_MASTER_OCW1); \
     else \
       outb (inb (PIC_PORT_SLAVE_IMR) & ~PIC_OCW1_MASK((n)-8), \
             PIC_PORT_SLAVE_OCW1); \
   } while (0)

#define DISABLE_IRQ(n) \
do { \
     if ((n) < 8) \
       outb (inb (PIC_PORT_MASTER_IMR) | PIC_OCW1_MASK(n), \
             PIC_PORT_MASTER_OCW1); \
     else \
       outb (inb (PIC_PORT_SLAVE_IMR) | PIC_OCW1_MASK((n)-8), \
             PIC_PORT_SLAVE_OCW1); \
   } while (0)

#define ACKNOWLEDGE_IRQ(n) \
do { \
     if ((n) < 8) \
       outb (PIC_OCW2_SPECIFIC_EOI(n), PIC_PORT_MASTER_OCW2); \
     else \
       { \
         outb (PIC_OCW2_SPECIFIC_EOI((n)-8), PIC_PORT_SLAVE_OCW2); \
         outb (PIC_OCW2_SPECIFIC_EOI(PIC_MASTER_IRQ2), PIC_PORT_MASTER_OCW2); \
       } \
   } while (0)

//-----------------------------------------------------------------------------

// Interrupt handlers must use C linkage.

extern "C" void irq0 ();
extern "C" void irq1 ();
extern "C" void irq2 ();
extern "C" void irq3 ();
extern "C" void irq4 ();
extern "C" void irq5 ();
extern "C" void irq6 ();
extern "C" void irq7 ();
extern "C" void irq8 ();
extern "C" void irq9 ();
extern "C" void irq10 ();
extern "C" void irq11 ();
extern "C" void irq12 ();
extern "C" void irq13 ();
extern "C" void irq14 ();
extern "C" void irq15 ();
extern "C" void APIC_timer_irq ();
extern "C" void APIC_spurious_irq ();
extern "C" void unhandled_interrupt (int num);
extern "C" void interrupt_handle(interrupt_data data);
extern "C" void sys_irq (void* esp);

//-----------------------------------------------------------------------------

#endif

// Local Variables: //
// mode: C++ //
// End: //
