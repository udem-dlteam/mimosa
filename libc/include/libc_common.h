#ifndef _LIBC_COMMON_HEADER

#define _LIBC_COMMON_HEADER 1

#include "include/libc_redirect.h"

#ifndef USE_HOST_LIBC

extern void libc_wr_char(int fd, char c);
extern void libc_wr_string(int fd, const char *s);
extern int libc_rd_char(int fd);

#endif

#ifdef ENABLE_LIBC_TRACE

#define libc_trace(msg) libc_trace_(msg)
extern void libc_trace_(const char *msg);

#else

#define libc_trace(msg)

#endif

#include "include/libc_link.h"

#endif /* libc_common.h */
