#include "include/libc_common.h"
#include "include/sys/time.h"

#ifndef USE_MIMOSA
extern "C" int gettimeofday(struct timeval *__restrict __tv,
                            struct timezone *__tz) {
#ifdef USE_LIBC_LINK

  return LIBC_LINK._gettimeofday(__tv, __tz);

#else

  libc_trace("gettimeofday");

#ifdef USE_HOST_LIBC

  return gettimeofday(__tv, __tz);

#else

#undef gettimeofday

  return gettimeofday(__tv, __tz);

#endif
#endif
}
#endif

int settimeofday(const struct timeval *__tv, const struct timezone *__tz) {
#ifdef USE_LIBC_LINK

  return LIBC_LINK._settimeofday(__tv, __tz);

#else

  libc_trace("settimeofday");

#ifdef USE_HOST_LIBC

#undef settimeofday

  return settimeofday(__tv, __tz);

#else

  // TODO: implement
  return 0;

#endif
#endif
}

int getitimer(int __which, struct itimerval *__value) {
#ifdef USE_LIBC_LINK

  return LIBC_LINK._getitimer(__which, __value);

#else

  libc_trace("getitimer");

#ifdef USE_HOST_LIBC

#undef getitimer

  return getitimer(__which, __value);

#else

  // TODO: implement
  return 0;

#endif
#endif
}

int setitimer(int __which, const struct itimerval *__restrict __new,
              struct itimerval *__restrict __old) {
#ifdef USE_LIBC_LINK

  return LIBC_LINK._setitimer(__which, __new, __old);

#else

  libc_trace("setitimer");

#ifdef USE_HOST_LIBC

#undef setitimer

  return setitimer(__which, __new, __old);

#else

  // TODO: implement
  return 0;

#endif
#endif
}

#ifndef USE_LIBC_LINK

void libc_init_sys_time(void) {}

#endif
