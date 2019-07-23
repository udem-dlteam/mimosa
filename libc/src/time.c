#include "include/libc_common.h"
#include "include/time.h"

clock_t clock(void) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._clock();

#else

  libc_trace("clock");

#ifdef USE_HOST_LIBC

#undef clock

  return clock();

#else

  /* TODO: implement */
  return 0;

#endif
#endif
}

time_t time(time_t *__timer) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._time(__timer);

#else

  libc_trace("time");

#ifdef USE_HOST_LIBC

#undef time

  return time(__timer);

#else

  /* TODO: implement */
  return 0;

#endif
#endif
}

#ifndef USE_LIBC_LINK

void libc_init_time(void) {
}

#endif
