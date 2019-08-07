#ifndef _UNISTD_HEADER

#define _UNISTD_HEADER 1

#include "include/libc_header.h"
#include "include/stddef.h"

#ifdef USE_MIMOSA

#include "../drivers/filesystem/include/vfs.h"

#else

#ifndef USE_HOST_LIBC

struct stat {
  int state;
};

#endif
#endif

extern char *REDIRECT_NAME(getcwd)(char *__buf, size_t __size);
extern int REDIRECT_NAME(mkdir)(const char *__pathname, mode_t __mode);
extern int REDIRECT_NAME(remove)(const char *__pathname);
extern int REDIRECT_NAME(lstat)(const char *__pathname, struct_stat *__buf);
extern int REDIRECT_NAME(stat)(const char *__pathname, struct_stat *__buf);
extern int REDIRECT_NAME(isatty)(int __fd);

#ifndef USE_LIBC_LINK

extern void libc_init_unistd(void);

#endif

#endif // unistd.h
