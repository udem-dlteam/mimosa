#ifndef _STDDEF_HEADER

#define _STDDEF_HEADER 1

#include "include/libc_header.h"

#ifndef USE_HOST_LIBC

#ifdef USE_MIMOSA

#ifndef __cplusplus
#include "include/wchar.h"
#endif
#include "general.h"

#else

#define CAST(type,value) ((type)(value))

typedef signed char int8;       // 8 bit signed integers
typedef signed short int16;     // 16 bit signed integers
typedef signed int int32;       // 32 bit signed integers
typedef signed long long int64; // 64 bit signed integers (gcc specific)

typedef unsigned char uint8;       // 8 bit unsigned integers
typedef unsigned short uint16;     // 16 bit unsigned integers
typedef unsigned int uint32;       // 32 bit unsigned integers
typedef unsigned long long uint64; // 64 bit unsigned integers (gcc specific)

typedef unsigned long size_t;

#endif

typedef int mode_t;

#define NULL 0

#endif

#endif // stddef.h
