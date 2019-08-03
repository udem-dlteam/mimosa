#include "include/libc_common.h"
#include "include/string.h"

#ifndef USE_MIMOSA
extern "C" void *REDIRECT_NAME(memcpy)(void *__restrict __dest, const void *__restrict __src,
                                       size_t __n) {
  return REDIRECT_NAME(memmove)(__dest, __src, __n);
}
#endif

void *REDIRECT_NAME(memmove)(void *__dest, const void *__src, size_t __n) {

  char *s = (char*)__src;
  char *d = (char*)__dest;
  if (s != d) {
    if (s < d) {
      char *e = s + __n;
      while (s < e) *d++ = *s++;
    } else {
      char *e = s;
      s += __n;
      d += __n;
      while (s > e) *--d = *--s;
    }
  }
  return __dest;
}

#ifndef USE_LIBC_LINK

void libc_init_string(void) {
}

#endif
