#ifndef _LIMITS_HEADER

#define _LIMITS_HEADER 1

#include "include/libc_header.h"

#define UCHAR_MAX  255U
#define USHRT_MAX  65535U
#define UINT_MAX   4294967295U
#if defined(__GNUC__) && __LONG_MAX__ == 9223372036854775807LL
#define ULONG_MAX  18446744073709551615UL
#else
#define ULONG_MAX  4294967295U
#endif
#define ULLONG_MAX 18446744073709551615ULL

#endif /* limits.h  */
