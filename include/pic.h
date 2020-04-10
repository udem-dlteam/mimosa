// file: "pic.h"

// Copyright (c) 2001 by Marc Feeley and Universit? de Montr?al, All
// Rights Reserved.
//
// Revision History
// 22 Sep 01  initial version (Marc Feeley)

#ifndef __PIC_H
#define __PIC_H

//-----------------------------------------------------------------------------

#include "general.h"

//-----------------------------------------------------------------------------

//
// Definitions for 8259 programmable interrupt controller.
//

// For Initialization Command Words (ICW)

#define PIC_PORT_MASTER_ICW1 0x20
#define PIC_PORT_SLAVE_ICW1 0xa0
#define PIC_PORT_MASTER_ICW2 0x21
#define PIC_PORT_SLAVE_ICW2 0xa1
#define PIC_PORT_MASTER_ICW3 0x21
#define PIC_PORT_SLAVE_ICW3 0xa1
#define PIC_PORT_MASTER_ICW4 0x21
#define PIC_PORT_SLAVE_ICW4 0xa1

#define PIC_ICW1(params) ((1 << 4) | (params))
#define PIC_ICW1_LTIM (1 << 3) // Level triggered mode
#define PIC_ICW1_SNGL (1 << 1) // Single PIC in system
#define PIC_ICW1_ICW4 (1 << 0) // ICW4 necessary

#define PIC_ICW2(offset) ((offset) << 3)

#define PIC_MASTER_ICW3(slaves) (slaves)
#define PIC_MASTER_ICW3_SLAVE(id) (1 << (id))

#define PIC_SLAVE_ICW3(id) (id)

#define PIC_ICW4(params) (params)
#define PIC_ICW4_SFNM (1 << 4)   // Operation in Special Fully Nested Mode
#define PIC_ICW4_BUF (1 << 3)    // Operation in Buffered Mode
#define PIC_ICW4_MASTER (1 << 2) // Master PIC
#define PIC_ICW4_AEOI (1 << 1)   // Automatic EOI
#define PIC_ICW4_8086 (1 << 0)   // Operation in 8086 mode

// For Operation Command Words (OCW)

#define PIC_PORT_MASTER_OCW1 0x21
#define PIC_PORT_SLAVE_OCW1 0xa1
#define PIC_PORT_MASTER_OCW2 0x20
#define PIC_PORT_SLAVE_OCW2 0xa0
#define PIC_PORT_MASTER_OCW3 0x20
#define PIC_PORT_SLAVE_OCW3 0xa0

#define PIC_OCW1_MASK(n) (1 << (n))

#define PIC_OCW2_NONSPECIFIC_EOI (1 << 5)
#define PIC_OCW2_SPECIFIC_EOI(level) ((3 << 5) + (level))

// For Interrupt Mask Register

#define PIC_PORT_MASTER_IMR 0x21
#define PIC_PORT_SLAVE_IMR 0xa1

// Interrupt request numbers

#define PIC_MASTER_IRQ0 0 // Programmable Interval Timer (counter 0)
#define PIC_MASTER_IRQ1 1 // Keyboard
#define PIC_MASTER_IRQ2 2 // Slave Programmable Interrupt Controller
#define PIC_MASTER_IRQ3 3 // Serial Port (COM2)
#define PIC_MASTER_IRQ4 4 // Serial Port (COM1)
#define PIC_MASTER_IRQ5 5 // ?
#define PIC_MASTER_IRQ6 6 // Floppy Disk Controller
#define PIC_MASTER_IRQ7 7 // Parallel Port (or a master "lost interrupt")

#define PIC_SLAVE_IRQ8 0  // Real Time Clock
#define PIC_SLAVE_IRQ9 1  // ?
#define PIC_SLAVE_IRQ10 2 // ?
#define PIC_SLAVE_IRQ11 3 // ?
#define PIC_SLAVE_IRQ12 4 // PS/2 Mouse
#define PIC_SLAVE_IRQ13 5 // Math Coprocessor
#define PIC_SLAVE_IRQ14 6 // Hard Disk Drive
#define PIC_SLAVE_IRQ15 7 // ? (or a slave "lost interrupt")

//-----------------------------------------------------------------------------

#endif

// Local Variables: //
// mode: C++ //
// End: //
