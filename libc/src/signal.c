#include "include/libc_common.h"
#include "include/signal.h"

__sighandler_t signal(int __sig, __sighandler_t __handler) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._signal(__sig, __handler);

#else

  libc_trace("signal");

#ifdef USE_HOST_LIBC

#undef signal

  return signal(__sig, __handler);

#else

  // TODO: implement
  return SIG_ERR;

#endif
#endif
}

#ifndef USE_LIBC_LINK

void libc_init_signal(void) {
}

#endif
