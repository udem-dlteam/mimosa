#include "include/libc_common.h"
#include "include/errno.h"

#ifndef USE_LIBC_LINK

#ifndef USE_HOST_LIBC

int int_errno = 0;

#endif

#endif

#ifndef USE_LIBC_LINK

void libc_init_errno(void) {
#ifdef USE_HOST_LIBC
  LIBC_LINK._errno = &errno;
#else
  LIBC_LINK._errno = &int_errno;
#endif
}

#endif
