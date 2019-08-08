#ifndef _SYS_TIME_HEADER

#define _SYS_TIME_HEADER 1

#include "include/libc_header.h"
#include "include/stddef.h"

#ifdef USE_MIMOSA

#include "chrono.h"

#else

#ifndef USE_HOST_LIBC

struct timeval {
  int32 tv_sec;  // seconds
  int32 tv_usec; // microseconds
};

struct timezone {
  int32 tz_minuteswest; // Minutes west of GMT
  int32 tz_dsttime;     // Nonzero if DST is ever in effect
};

#define ITIMER_REAL    0
#define ITIMER_VIRTUAL 1
#define ITIMER_PROF    2

struct itimerval {
  struct timeval it_interval;
  struct timeval it_value;
};

#endif

#endif

extern int REDIRECT_NAME(gettimeofday)(struct timeval *__restrict __tv,
                                       struct timezone *__tz);

extern int REDIRECT_NAME(settimeofday)(const struct timeval *__tv,
                                       const struct timezone *__tz);

extern int REDIRECT_NAME(getitimer)(int __which,
                                    struct itimerval *__value);

extern int REDIRECT_NAME(setitimer)(int __which,
                                    const struct itimerval *__restrict __new,
                                    struct itimerval *__restrict __old);

#ifndef USE_LIBC_LINK

extern void libc_init_sys_time(void);

#endif

#endif // sys/time.h
