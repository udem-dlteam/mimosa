#ifndef _STDDEF_HEADER

#define _STDDEF_HEADER 1

#include "include/libc_header.h"

#ifndef USE_HOST_LIBC

#ifdef USE_MIMOSA
#include "general.h"
#else
typedef unsigned long size_t;
#endif

typedef int mode_t;

#define NULL 0

#endif

#endif /* stddef.h */
