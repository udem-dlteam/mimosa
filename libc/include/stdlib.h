#ifndef _STDLIB_HEADER

#define _STDLIB_HEADER 1

#include "include/libc_header.h"
#include "include/stddef.h"

extern void *malloc(size_t __size);

extern void free(void *__ptr);

extern void exit(int __status);

//extern char **environ;

extern char *getenv(const char *__name);

extern int system(const char *__command);

#ifndef USE_LIBC_LINK

extern void libc_init_stdlib(void);

#endif

#endif /* stdlib.h */
