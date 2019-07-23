#include "include/libc_common.h"
#include "include/setjmp.h"

int setjmp(jmp_buf __env) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._setjmp(__env);

#else

  libc_trace("setjmp");

#ifdef USE_HOST_LIBC

#undef setjmp

  return setjmp(__env);

#else

  /* TODO: implement */
  return 0;

#endif
#endif
}

void longjmp(jmp_buf __env, int __val) {

#ifdef USE_LIBC_LINK

  LIBC_LINK._longjmp(__env, __val);

#else

  libc_trace("longjmp");

#ifdef USE_HOST_LIBC

#undef longjmp

  exit(0); /* don't actually longjmp, because setjmp definition above is bogus */
  longjmp(__env, __val);

#else

  /* TODO: implement */
  for (;;) ;

#endif
#endif
}

#ifndef USE_LIBC_LINK

void libc_init_setjmp(void) {
}

#endif
