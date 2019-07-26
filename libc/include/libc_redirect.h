#ifndef _LIBC_REDIRECT_HEADER

#define _LIBC_REDIRECT_HEADER 1

#ifdef REDIRECT_PREFIX

#define REDIRECT_NAME(name)CONCAT(REDIRECT_PREFIX,name)
#define CONCAT(a,b)CONCAT2(a,b)
#define CONCAT2(a,b)a##b

/* dirent.h */
#define opendir REDIRECT_NAME(opendir)
#define readdir REDIRECT_NAME(readdir)
#define closedir REDIRECT_NAME(closedir)

/* errno.h */
#if 0
#define errno REDIRECT_NAME(errno)
#endif

/* math.h */
#define acos REDIRECT_NAME(acos)
#define acosh REDIRECT_NAME(acosh)
#define asin REDIRECT_NAME(asin)
#define asinh REDIRECT_NAME(asinh)
#define atan REDIRECT_NAME(atan)
#define atan2 REDIRECT_NAME(atan2)
#define atanh REDIRECT_NAME(atanh)
#define ceil REDIRECT_NAME(ceil)
#define cos REDIRECT_NAME(cos)
#define cosh REDIRECT_NAME(cosh)
#define exp REDIRECT_NAME(exp)
#define expm1 REDIRECT_NAME(expm1)
#define fabs REDIRECT_NAME(fabs)
#define floor REDIRECT_NAME(floor)
#define hypot REDIRECT_NAME(hypot)
#define ilogb REDIRECT_NAME(ilogb)
#define log REDIRECT_NAME(log)
#define log1p REDIRECT_NAME(log1p)
#define modf REDIRECT_NAME(modf)
#define pow REDIRECT_NAME(pow)
#define sin REDIRECT_NAME(sin)
#define sinh REDIRECT_NAME(sinh)
#define sqrt REDIRECT_NAME(sqrt)
#define tan REDIRECT_NAME(tan)
#define tanh REDIRECT_NAME(tanh)
#define scalbn REDIRECT_NAME(scalbn)

/* setjmp.h */
#define setjmp REDIRECT_NAME(setjmp)
#define longjmp REDIRECT_NAME(longjmp)

/* stdio.h */
#define fopen REDIRECT_NAME(fopen)
#define fdopen REDIRECT_NAME(fdopen)
#define fread REDIRECT_NAME(fread)
#define fwrite REDIRECT_NAME(fwrite)
#define fclose REDIRECT_NAME(fclose)
#define fflush REDIRECT_NAME(fflush)
#define fseek REDIRECT_NAME(fseek)
#define ftell REDIRECT_NAME(ftell)
#define ferror REDIRECT_NAME(ferror)
#define feof REDIRECT_NAME(ferror)
#define clearerr REDIRECT_NAME(clearerr)
#define setbuf REDIRECT_NAME(setbuf)
#define rename REDIRECT_NAME(rename)
#define fprintf REDIRECT_NAME(fprintf)
#define printf REDIRECT_NAME(printf)
#if 0
#define stdin REDIRECT_NAME(stdin)
#define stdout REDIRECT_NAME(stdout)
#define stderr REDIRECT_NAME(stderr)
#endif

/* stdlib.h */
#define malloc REDIRECT_NAME(malloc)
#define free REDIRECT_NAME(free)
#define exit REDIRECT_NAME(exit)
#define getenv REDIRECT_NAME(getenv)
#define system REDIRECT_NAME(system)

/* string.h */
#define memcpy REDIRECT_NAME(memcpy)
#define memmove REDIRECT_NAME(memmove)

/* termios.h */
#define tcgetattr REDIRECT_NAME(tcgetattr)
#define tcsetattr REDIRECT_NAME(tcsetattr)
#define cfsetospeed REDIRECT_NAME(cfsetospeed)
#define cfsetispeed REDIRECT_NAME(cfsetispeed)

/* time.h */
#define clock REDIRECT_NAME(clock)
#define time REDIRECT_NAME(time)

/* unistd.h */
#define getcwd REDIRECT_NAME(getcwd)
#define mkdir REDIRECT_NAME(mkdir)
#define remove REDIRECT_NAME(remove)
#define stat REDIRECT_NAME(stat)
#define lstat REDIRECT_NAME(lstat)

#endif

#endif /* libc_redirect.h */
