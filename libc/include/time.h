#ifndef _TIME_HEADER

#define _TIME_HEADER 1

#include "include/libc_header.h"

#ifndef USE_HOST_LIBC

typedef int32 clock_t;
typedef int32 time_t;
typedef int32 clockid_t;

struct timespec {
  int32 ts_sec;  // seconds
  int32 ts_nsec; // nanoseconds
};

#define CLOCK_REALTIME  0
#define CLOCK_MONOTONIC 1

#endif

extern clock_t clock(void);

extern time_t time(time_t *__timer);

extern int nanosleep(const struct timespec *__requested_time,
                     struct timespec *__remaining);

extern int clock_getres(clockid_t __clock_id, struct timespec *__res);
extern int clock_gettime(clockid_t __clock_id, struct timespec *__tp);
extern int clock_settime(clockid_t __clock_id, const struct timespec *__tp);

#ifndef USE_LIBC_LINK

extern void libc_init_time(void);

#endif

#endif // time.h
