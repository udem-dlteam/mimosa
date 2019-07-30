#include "include/libc_common.h"
#include "include/sys/resource.h"

int getrusage(int __who, struct rusage *__usage) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._getrusage(__who, __usage);

#else

  libc_trace("getrusage");

#ifdef USE_HOST_LIBC

#undef getrusage

  return getrusage(__who, __usage);

#else

  // TODO: implement
  return 0;

#endif
#endif
}

#ifndef USE_LIBC_LINK

void libc_init_sys_resource(void) {
}

#endif
