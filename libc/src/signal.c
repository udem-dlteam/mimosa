#include "include/libc_common.h"
#include "include/signal.h"

#ifndef USE_LIBC_LINK

static __sighandler_t signal_handlers[32];

void send_signal(int sig) {

  __sighandler_t sh = signal_handlers[sig];

  if (sh != SIG_IGN) {
    if (sh != SIG_DFL) {
      signal_handlers[sig] = SIG_IGN; // reset handler (System V semantics)
      sh(sig);
    } else {
      switch (sig) {
      default:
        // for now all default signal handlers are noops
        ;
      }
    }
  }
}

#endif

__sighandler_t REDIRECT_NAME(signal)(int __sig, __sighandler_t __handler) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._signal(__sig, __handler);

#else

  // libc_trace("signal");

#ifdef USE_HOST_LIBC

  return signal(__sig, __handler);

#else

  {
    __sighandler_t old = signal_handlers[__sig];
    signal_handlers[__sig] = __handler;
    return old;
  }

#endif
#endif
}

#ifndef USE_LIBC_LINK

void libc_init_signal(void) {
  int i;
  for (i=0; i<32; i++) {
    signal_handlers[i] = SIG_DFL;
  }
}

#endif
