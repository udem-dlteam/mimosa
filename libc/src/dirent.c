#include "include/libc_common.h"
#include "include/dirent.h"

DIR *REDIRECT_NAME(opendir)(const char *__name) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._opendir(__name);

#else

  libc_trace("opendir");

#ifdef USE_HOST_LIBC

  return opendir(__name);

#else

  return opendir(__name);

#endif
#endif
}

struct dirent *REDIRECT_NAME(readdir)(DIR *__dirp) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._readdir(__dirp);

#else

  libc_trace("readdir");

#ifdef USE_HOST_LIBC

  return readdir(__dirp);

#else

  return readdir(__dirp);

#endif
#endif
}

int REDIRECT_NAME(closedir)(DIR *__dirp) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._closedir(__dirp);

#else

  libc_trace("closedir");

#ifdef USE_HOST_LIBC

  return closedir(__dirp);

#else

  return closedir(__dirp);

#endif
#endif
}

#ifndef USE_LIBC_LINK

void libc_init_dirent(void) {
}

#endif
