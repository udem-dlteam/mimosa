#include "include/libc_common.h"
#include "include/stdlib.h"

void *malloc(size_t __size) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._malloc(__size);

#else

  libc_trace("malloc");

#ifdef USE_HOST_LIBC

#undef malloc

  return malloc(__size);

#else

  // TODO: implement

  {
#define MB (1<<20)
#define HEAP_SIZE 10*MB // needs to be at least 5*MB

    static char heap[HEAP_SIZE];
    static int alloc = HEAP_SIZE;

    size_t bytes = (__size + 7) & ~7;

    if (bytes > alloc) {
      libc_trace("heap_overflow");
      return NULL;
    } else {
      alloc -= bytes;
      return (void*)(heap+alloc);
    }
  }

#endif
#endif
}

void free(void *__ptr) {

#ifdef USE_LIBC_LINK

  LIBC_LINK._free(__ptr);

#else

  libc_trace("free");

#ifdef USE_HOST_LIBC

#undef free

  free(__ptr);

#else

  // TODO: implement

#endif
#endif
}

void exit(int __status) {

#ifdef USE_LIBC_LINK

  LIBC_LINK._exit(__status);

#else

  libc_trace("exit");

#ifdef USE_HOST_LIBC

#undef exit

  exit(__status);

#else

  // TODO: implement
  for (;;) ;

#endif
#endif

  // NOTREACHED
}

char *getenv(const char *__name) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._getenv(__name);

#else

  libc_trace("getenv");

#ifdef USE_HOST_LIBC

#undef getenv

  return getenv(__name);

#else

  // TODO: implement
  return NULL;

#endif
#endif
}

int system(const char *__command) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._system(__command);

#else

  libc_trace("system");

#ifdef USE_HOST_LIBC

#undef system

  return system(__command);

#else

  // TODO: implement
  return 0;

#endif
#endif
}

#ifndef USE_LIBC_LINK

void libc_init_stdlib(void) {
}

#endif
