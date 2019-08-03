#ifndef _STDLIB_HEADER

#define _STDLIB_HEADER 1

#include "include/libc_header.h"
#include "include/stddef.h"

extern void *REDIRECT_NAME(malloc)(size_t __size);

extern void REDIRECT_NAME(free)(void *__ptr);

extern void REDIRECT_NAME(exit)(int __status);

//extern char **environ;

extern char *REDIRECT_NAME(getenv)(const char *__name);

extern int REDIRECT_NAME(system)(const char *__command);

#ifndef USE_LIBC_LINK

extern void libc_init_stdlib(void);

#endif

#endif // stdlib.h
