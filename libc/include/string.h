#ifndef _STRING_HEADER

#define _STRING_HEADER 1

#include "include/libc_header.h"
#include "include/stddef.h"

#ifdef __cplusplus
extern "C"
#endif
void *REDIRECT_NAME(memcpy)(void *__restrict __dest, const void *__restrict __src,
                            size_t __n);

extern void *REDIRECT_NAME(memmove)(void *__dest, const void *__src, size_t __n);

#ifndef USE_LIBC_LINK

extern void libc_init_string(void);

#endif

#endif // string.h
