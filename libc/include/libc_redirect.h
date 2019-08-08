#ifndef _LIBC_REDIRECT_HEADER

#define _LIBC_REDIRECT_HEADER 1

#ifdef REDIRECT_PREFIX

#define REDIRECT_NAME(name)CONCAT(REDIRECT_PREFIX,name)
#define CONCAT(a,b)CONCAT2(a,b)
#define CONCAT2(a,b)a##b

#else

#define REDIRECT_NAME(name)name

#endif

#endif // libc_redirect.h
