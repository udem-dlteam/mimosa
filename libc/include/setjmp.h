#ifndef _SETJMP_HEADER

#define _SETJMP_HEADER 1

#include "include/libc_header.h"

#ifndef USE_HOST_LIBC

typedef int jmp_buf[6];

#endif

extern int setjmp(jmp_buf __env);
extern void longjmp(jmp_buf __env, int __val);

#ifndef USE_LIBC_LINK

extern void libc_init_setjmp(void);

#endif

#endif // setjmp.h
