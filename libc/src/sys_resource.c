#include "include/libc_common.h"
#include "include/sys/resource.h"

#ifdef USE_MIMOSA

#include "general.h"
#include "chrono.h"

#endif

int getrusage(int __who, struct rusage *__usage) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._getrusage(__who, __usage);

#else

  libc_trace("getrusage");

#ifdef USE_HOST_LIBC

#undef getrusage

  return getrusage(__who, __usage);

#else

#undef gettimeofday

  gettimeofday(&__usage->ru_utime, NULL);

  __usage->ru_stime.tv_sec = 0;
  __usage->ru_stime.tv_usec = 0;
  __usage->ru_minflt = 0;
  __usage->ru_majflt = 0;

  return 0;

#endif
#endif
}

#ifndef USE_LIBC_LINK

void libc_init_sys_resource(void) {
}

#endif
