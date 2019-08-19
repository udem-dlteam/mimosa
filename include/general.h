// file: "general.h"

// Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 22 Sep 01  initial version (Marc Feeley)

#ifndef GENERAL_H
#define GENERAL_H

//-----------------------------------------------------------------------------

#define NULL 0

#define TRUE 1
#define FALSE 0

#define CAST(type,value) ((type)(value))

typedef signed char int8;       // 8 bit signed integers
typedef signed short int16;     // 16 bit signed integers
typedef signed int int32;       // 32 bit signed integers
typedef signed long long int64; // 64 bit signed integers (gcc specific)

typedef unsigned char uint8;       // 8 bit unsigned integers
typedef unsigned short uint16;     // 16 bit unsigned integers
typedef unsigned int uint32;       // 32 bit unsigned integers
typedef unsigned long long uint64; // 64 bit unsigned integers (gcc specific)

//typedef __wchar_t unicode_char;
typedef wchar_t unicode_char;
//typedef typeof (*L"") unicode_char;
typedef unicode_char* unicode_string;

typedef char native_char;
typedef native_char* native_string;

typedef uint32 size_t;
typedef int32 ssize_t;

#define as_uint16(x) \
((CAST(uint16,(x)[1])<<8)+(x)[0])

#define as_uint32(x) \
((((((CAST(uint32,(x)[3])<<8)+(x)[2])<<8)+(x)[1])<<8)+(x)[0])

#define as_uint8(x, i) (((x) & ((0xFF) << ((i) << 3))) >> ((i) << 3))

//-----------------------------------------------------------------------------

// Error codes.

typedef int32 error_code;

#define IN_PROGRESS 1
#define NO_ERROR 0
#define EOF_ERROR (-1)
#define MEM_ERROR (-2)
#define FNF_ERROR (-3)
#define UNIMPL_ERROR (-4)
#define DISK_OUT_OF_SPACE (-5)
#define ARG_ERROR (-6)
#define UNKNOWN_ERROR (-7)
#define PERMISSION_ERROR (-8)
#define NOT_A_FOLDER_ERR (-9)
#define EXISTS_ERR (-10)

#define NOP() do { __asm__ __volatile__ ("NOP" : : : "memory");} while(0)

#define HAS_NO_ERROR(x) ((x) >= 0)
#define ERROR(x) ((x) < 0)

//-----------------------------------------------------------------------------

// Select implementations.

// For the interval timer, the programmable interval timer (PIT) IRQ0
// interrupt or the local APIC timer can be used.  The PIT time
// interval can be configured using a 1 byte or 2 byte count.  Note
// that "bochs" does not implement the 1 byte mode.

#define USE_PIT_FOR_TIMER
// #define USE_APIC_FOR_TIMER

#ifdef USE_PIT_FOR_TIMER
// #define USE_PIT_1_BYTE_COUNT
#endif

// For keeping track of elapsed time we can use the real-time clock
// (RTC) IRQ8 interrupt or the time stamp counter (TSC) which exists
// on CPUs above the 486.  Note that "bochs" has a bug which prevents
// the RTC IRQ8 interrupt and PIT IRQ0 interrupt to be used
// simultaneously.

// #define USE_IRQ8_FOR_TIME
#define USE_TSC_FOR_TIME

#ifdef USE_IRQ8_FOR_TIME
#define IRQ8_COUNTS_PER_SEC 128
#endif

// For the keyboard and mouse, IRQ1 and IRQ12 are used respectively.

#define USE_IRQ1_FOR_KEYBOARD
#define USE_IRQ12_FOR_MOUSE

// For the IDE controllers 0 and 1, IRQ14 and IRQ15 are used respectively.

#define USE_IRQ14_FOR_IDE0
#define USE_IRQ15_FOR_IDE1

// UART requires IRQ4 and IRQ3
#define USE_IRQ3_FOR_UART
#define USE_IRQ4_FOR_UART

// A thread's context can be restored with an "iret" instruction or a
// "ret" instruction.  For some unexplained reason the latest AMD
// Athlon processors cause an "invalid TSS" exception when the "iret"
// instruction is used.

// #define USE_IRET_FOR_RESTORE_CONTEXT
#define USE_RET_FOR_RESTORE_CONTEXT

// #define REMOTE_COM
#define GAMBIT_REPL
// #define MIMOSA_REPL

#define SHOW_BOOT_TIME

#ifdef GAMBIT_REPL
#ifdef MIMOSA_REPL
#error "Only one REPL should be used"
#endif
#endif

#ifndef GAMBIT_REPL
#ifndef MIMOSA_REPL
#error "At least a REPL should be defined"
#endif
#endif

// #define STREAM_STDOUT_TO_DEBUG_CONSOLE
// #define SHOW_INTERRUPTS
// #define SHOW_TIMER_INTERRUPTS
//#define SHOW_CPU_INFO
// #define SHOW_IDE_INFO
// #define SHOW_DISK_INFO
#define CHECK_ASSERTIONS
// #define PRINT_ASSERTIONS
#define USE_CACHE_BLOCK_MAID
#define USE_BLOCK_REF_COUNTER_FREE
#define SHOW_UART_MESSAGES
// #define RED_PANIC_SCREEN
#define ENABLE_DEBUG_WRITE
#define ENABLE_DEBUG_MARKER
#define ENABLE_MOUSE

// BUSY_WAIT_INSTEAD_OF_SLEEP uses a simple for loop
// to perform sleep operations. This is bad, but it might
// help to debug some timer problems
// #define BUSY_WAIT_INSTEAD_OF_SLEEP


// #define ENABLE_LIBC_TRACE
//-----------------------------------------------------------------------------

#ifndef USE_CACHE_BLOCK_MAID
#ifndef USE_BLOCK_REF_COUNTER_FREE
#error "A cache block cleaning strategy must be defined"
#endif
#endif

#endif

// Local Variables: //
// mode: C++ //
// End: //
