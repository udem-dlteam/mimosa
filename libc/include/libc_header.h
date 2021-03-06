#ifndef _LIBC_HEADER_HEADER

#define _LIBC_HEADER_HEADER 1

#ifdef USE_HOST_LIBC

#include <dirent.h>
#include <errno.h>
#include <math.h>
#include <setjmp.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>

#endif

#ifdef GAMBIT_GSTATE

#define ___DONT_HAVE_LIMITS_H
#define ___DONT_HAVE_WCHAR_H
#define ___DONT_HAVE_FLOAT_H
#define ___DONT_HAVE_SIGNAL_H
#define ___DONT_HAVE_NEW
#define ___DONT_HAVE_MATH_H

#ifndef USE_LIBC_LINK
#include "modifiedgambit.h"
#else
#include "gambit.h"
#endif

//#undef ___GSTATE
//#define ___GSTATE ___local_gstate
volatile extern struct ___global_state_struct *___local_gstate;

#endif

#include "include/libc_redirect.h"

#endif // libc_header.h
