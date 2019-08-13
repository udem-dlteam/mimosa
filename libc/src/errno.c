#include "include/libc_common.h"
#include "include/errno.h"

#ifndef USE_LIBC_LINK

#ifndef USE_HOST_LIBC

int errno = 0;

#endif

void libc_init_errno(void) {
  LIBC_LINK._errno = &errno;
}

#endif
