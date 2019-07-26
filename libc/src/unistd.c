#include "include/libc_common.h"
#include "include/unistd.h"

char *getcwd(char *__buf, size_t __size) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._getcwd(__buf, __size);

#else

  libc_trace("getcwd");

#ifdef USE_HOST_LIBC

#undef getcwd

  return getcwd(__buf, __size);

#else

  /* TODO: implement */
  __buf[0] = '/';
  __buf[1] = '\0';
  return __buf;

#endif
#endif
}

int mkdir(const char *__pathname, mode_t __mode) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._mkdir(__pathname, __mode);

#else

  libc_trace("mkdir");

#ifdef USE_HOST_LIBC

#undef mkdir

  return mkdir(__pathname, __mode);

#else

  /* TODO: implement */
  return 0;

#endif
#endif
}

int remove(const char *__pathname) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._remove(__pathname);

#else

  libc_trace("remove");

#ifdef USE_HOST_LIBC

#undef remove

  return remove(__pathname);

#else

  /* TODO: implement */
  return 0;

#endif
#endif
}

int lstat(const char *__pathname, struct_stat *__buf) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._lstat(__pathname, __buf);

#else

  libc_trace("lstat");

#ifdef USE_HOST_LIBC

#undef lstat

  return lstat(__pathname, __buf);

#else

  /* TODO: implement */
  return 0;

#endif
#endif
}

int stat(const char *__pathname, struct_stat *__buf) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._stat(__pathname, __buf);

#else

  libc_trace("stat");

#ifdef USE_HOST_LIBC

#undef stat

  return stat(__pathname, __buf);

#else

  /* TODO: implement */
  return 0;

#endif
#endif
}

#ifndef USE_LIBC_LINK

void libc_init_unistd(void) {
}

#endif
