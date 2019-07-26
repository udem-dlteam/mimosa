#ifndef _TIME_HEADER

#define _TIME_HEADER 1

#include "include/libc_header.h"

#ifndef USE_HOST_LIBC

typedef int clock_t;
typedef int time_t;

#endif

extern clock_t clock(void);

extern time_t time(time_t *__timer);

#ifndef USE_LIBC_LINK

extern void libc_init_time(void);

#endif

#endif /* time.h */
