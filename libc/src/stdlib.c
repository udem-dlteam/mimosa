#include "include/libc_common.h"
#include "include/stdlib.h"

#ifdef USE_MIMOSA

#include "heap.h"

#endif

void *REDIRECT_NAME(malloc)(size_t __size) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._malloc(__size);

#else

  libc_trace("malloc");

#ifdef USE_HOST_LIBC

  return malloc(__size);

#else

  // TODO: implement

#ifdef USE_MIMOSA

  return heap_malloc(&appheap, __size);

#else

  {
#define MB (1<<20)
#define HEAP_SIZE 40*MB // needs to be at least 5*MB

    debug_write("MANUAL HEAP ALLOC");
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
#endif
}

void REDIRECT_NAME(free)(void *__ptr) {

#ifdef USE_LIBC_LINK

  LIBC_LINK._free(__ptr);

#else

  libc_trace("free");

#ifdef USE_HOST_LIBC

  free(__ptr);

#else

#ifdef USE_MIMOSA

  return heap_free(&appheap, __ptr);

#else
  // TODO: implement

#endif

#endif
#endif
}

void REDIRECT_NAME(exit)(int __status) {

#ifdef USE_LIBC_LINK

  LIBC_LINK._exit(__status);

#else

  libc_trace("exit");

#ifdef USE_HOST_LIBC

  exit(__status);

#else

  // TODO: implement
  for (;;) ;

#endif
#endif

  // NOTREACHED
}

char *REDIRECT_NAME(getenv)(const char *__name) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._getenv(__name);

#else

  libc_trace("getenv");

#ifdef USE_HOST_LIBC

  return getenv(__name);

#else

  // TODO: implement
  return NULL;

#endif
#endif
}

int REDIRECT_NAME(system)(const char *__command) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._system(__command);

#else

  libc_trace("system");

#ifdef USE_HOST_LIBC

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
