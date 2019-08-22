#ifndef _ERRNO_HEADER

#define _ERRNO_HEADER 1

#include "include/libc_header.h"

#define ENOENT 2  // No such file or directory
#define EINTR  4  // Interrupted system call
#define EAGAIN 11 // Try again
#define EEXIST 17 // File exists
#define ENOTDIR 20 // Not a directory
#define	ERANGE 34 // Math result not representable

extern int errno;

#ifdef USE_LIBC_LINK

#define errno *LIBC_LINK._errno

#else

#ifndef USE_HOST_LIBC

extern void libc_init_errno(void);

#endif

#endif

#endif // errno.h
