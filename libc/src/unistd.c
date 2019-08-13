#include "include/libc_common.h"
#include "include/unistd.h"
#include "include/errno.h"

int REDIRECT_NAME(chdir)(const char *__path) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._chdir(__path);

#else

  libc_trace("chdir");

#ifdef USE_HOST_LIBC

  return chdir(__path);

#else

  // TODO: implement
  return 0;

#endif
#endif
}

char *REDIRECT_NAME(getcwd)(char *__buf, size_t __size) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._getcwd(__buf, __size);

#else

  libc_trace("getcwd");

#ifdef USE_HOST_LIBC

  return getcwd(__buf, __size);

#else

  // TODO: implement
  __buf[0] = '/';
  __buf[1] = 'd';
  __buf[2] = 's';
  __buf[3] = 'k';
  __buf[4] = '1';
  __buf[5] = '\0';
  return __buf;

#endif
#endif
}

int REDIRECT_NAME(mkdir)(const char *__pathname, mode_t __mode) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._mkdir(__pathname, __mode);

#else

  libc_trace("mkdir");

#ifdef USE_HOST_LIBC

  return mkdir(__pathname, __mode);

#else

  // TODO: implement
  errno = ENOENT;
  return -1;

#endif
#endif
}

int REDIRECT_NAME(remove)(const char *__pathname) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._remove(__pathname);

#else

  libc_trace("remove");

#ifdef USE_HOST_LIBC

  return remove(__pathname);

#else

  // TODO: implement
  errno = ENOENT;
  return -1;

#endif
#endif
}

int REDIRECT_NAME(lstat)(const char *__pathname, struct stat *__buf) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._lstat(__pathname, __buf);

#else

  libc_trace("lstat");

#ifdef USE_HOST_LIBC

  return lstat(__pathname, __buf);

#else

  // TODO: implement
  errno = ENOENT;
  return -1;

#endif
#endif
}

int REDIRECT_NAME(stat)(const char *__pathname, struct stat *__buf) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._stat(__pathname, __buf);

#else

  libc_trace("stat");

#ifdef USE_HOST_LIBC

  return stat(__pathname, __buf);

#else

  // TODO: implement
  errno = ENOENT;
  return -1;

#endif
#endif
}

int REDIRECT_NAME(isatty)(int __fd) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._isatty(__fd);

#else

  libc_trace("isatty");

#ifdef USE_HOST_LIBC

  return isatty(__fd);

#else

  // TODO: implement
  return 0;

#endif
#endif
}

#ifndef USE_LIBC_LINK

void libc_init_unistd(void) {
}

#endif
