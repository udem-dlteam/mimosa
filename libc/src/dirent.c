#include "include/libc_common.h"
#include "include/dirent.h"

DIR *opendir(const char *__name) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._opendir(__name);

#else

  libc_trace("opendir");

#ifdef USE_HOST_LIBC

#undef opendir

  return opendir(__name);

#else

  // TODO: implement
  return NULL;

#endif
#endif
}

struct dirent *readdir(DIR *__dirp) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._readdir(__dirp);

#else

  libc_trace("readdir");

#ifdef USE_HOST_LIBC

#undef readdir

  return readdir(__dirp);

#else

  // TODO: implement
  return NULL;

#endif
#endif
}

int closedir(DIR *__dirp) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._closedir(__dirp);

#else

  libc_trace("closedir");

#ifdef USE_HOST_LIBC

#undef closedir

  return closedir(__dirp);

#else

  // TODO: implement
  return 0;

#endif
#endif
}

#ifndef USE_LIBC_LINK

void libc_init_dirent(void) {
}

#endif
