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

//-----------------------------------------------------------------------------

// Error handling.

void reboot();

void panic(unicode_string msg);

//-----------------------------------------------------------------------------

// Math routines.

uint8 log2 (uint32 n);

//-----------------------------------------------------------------------------

// Memory management.

void* kmalloc(size_t size);
void kfree(void* ptr);

extern "C" void* memcpy(void* dest, const void* src, size_t n);

// ----------------------------------------------------------------------------
// Strings


native_string copy_without_trailing_spaces(uint8* src, native_string dst,
                                                  uint32 n);

int16 kstrcmp(native_string a, native_string b);

native_string kstrconcat(native_string a, native_string b);

uint32 kstrlen(native_string a);
//-----------------------------------------------------------------------------

// Execution of global constructors and destructors.

void __do_global_ctors ();
void __do_global_dtors ();

//-----------------------------------------------------------------------------

// Runtime library entry point.

extern "C"
void __rtlib_entry ();

void __rtlib_setup();


int main ();

//-----------------------------------------------------------------------------
// Global objects
//-----------------------------------------------------------------------------


// ----------------------------------------------------------------------------
// Gambit communications

#define GAMBIT_KEYBOARD_INT 0x1
#define GAMBIT_UART_INT 0x2

uint8 send_gambit_int(uint8 int_no);

uint8 send_gambit_int(uint8 int_no, uint8 arg);

uint8 send_gambit_int(uint8 int_no, uint8 arg1, uint8 arg2);

//-----------------------------------------------------------------------------

#endif

// Local Variables: //
// mode: C++ //
// End: //
