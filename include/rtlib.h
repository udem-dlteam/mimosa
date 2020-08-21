// file: "rtlib.h"

// Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 22 Sep 01  initial version (Marc Feeley)

#ifndef __RTLIB_H
#define __RTLIB_H

//-----------------------------------------------------------------------------

#include "general.h"
#include "intr.h"

#define MEMORY_ZONES_COUNT_START 0x21000
#define MEMORY_ZONES_START (0x21000 + (8 * sizeof(uint32)))

#define MEMORY_ZONE_USABLE 1
#define MEMORY_ZONE_RSVD 2
#define MEMORY_ZONE_ACPI_RECLAIM 3
#define MEMORY_ZONE_ACPI_NVS 4
#define MEMORY_ZONE_BAD 5
#define MEMORY_ZONE_CONTAINS(mz_t, addr)                                       \
  (((mz_t.base) <= (addr)) && ((mz_t.base) + (mz_t.length) >= (addr)))

typedef struct {
  uint64 base;     // Base address of the memory zone
  uint64 length;   // length of the zone (in bytes)
  uint32 type;     // type of the memory zone (see definitions)
  uint32 acpi_ext; // ACPI extension. This field is ignored on Mimosa
  // and may not contain correct information
} memory_zone;

typedef struct {
  uint32 processor;
  uint32 dummy;
  uint32 features;
} cpu;

//----------------------------------------------------------------------------
// Globals

extern cpu c;

//-----------------------------------------------------------------------------

// Error handling.

void reboot();

void panic(unicode_string msg);

void panic(unicode_string msg, interrupt_data *data);

//-----------------------------------------------------------------------------

// Math routines.

uint8 log2(uint32 n);

//-----------------------------------------------------------------------------

// Memory management.

void *kmalloc(size_t size);
void kfree(void *ptr);

extern "C" void *memcpy(void *dest, const void *src, size_t n);

// ----------------------------------------------------------------------------
// Strings

native_string copy_without_trailing_spaces(uint8 *src, native_string dst,
                                           uint32 n);

int16 kstrcmp(native_string a, native_string b);

native_string kstrconcat(native_string a, native_string b);

uint32 kstrlen(native_string a);

unicode_string ptr_to_str(void *x, unicode_char *buffer);
//-----------------------------------------------------------------------------

// Execution of global constructors and destructors.

void __do_global_ctors();
void __do_global_dtors();

//-----------------------------------------------------------------------------

// Runtime library entry point.

extern "C" void __rtlib_entry();

void __rtlib_setup();

int main();

//-----------------------------------------------------------------------------
// Global objects
//-----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
// Gambit communications

#define GAMBIT_KEYBOARD_INT 0x1
#define GAMBIT_UART_INT 0x2
#define GAMBIT_IDE_INT 0x3

#define FLOW_CONTROLLED(int_no) ((int_no) == GAMBIT_IDE_INT)

void cut_ide_support();

bool has_cut_ide_support();

bool bridge_up();

uint8 send_gambit_int(uint8 int_no, uint8 *params, uint8 len);

//-----------------------------------------------------------------------------

#endif

// Local Variables: //
// mode: C++ //
// End: //
