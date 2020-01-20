#ifndef _SETJMP_HEADER

#define _SETJMP_HEADER 1

#include "include/libc_header.h"
#include "include/libc_redirect.h"

#ifndef USE_HOST_LIBC

typedef int jmp_buf[6];

#endif

extern int REDIRECT_NAME(setjmp)(jmp_buf __env);
extern void REDIRECT_NAME(longjmp)(jmp_buf __env, int __val);

#ifndef USE_LIBC_LINK

extern void libc_init_setjmp(void);

#endif

#endif // setjmp.h
