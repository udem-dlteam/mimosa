#ifndef _UNISTD_HEADER

#define _UNISTD_HEADER 1

#include "include/libc_header.h"
#include "include/stddef.h"

#ifndef USE_HOST_LIBC

struct stat {
  int state;
};

#endif

extern char *getcwd(char *__buf, size_t __size);
extern int mkdir(const char *__pathname, mode_t __mode);
extern int remove(const char *__pathname);
extern int lstat(const char *__pathname, struct_stat *__buf);
extern int stat(const char *__pathname, struct_stat *__buf);

#ifndef USE_LIBC_LINK

extern void libc_init_unistd(void);

#endif

#endif // unistd.h
