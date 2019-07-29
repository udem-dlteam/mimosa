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
#include <time.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/time.h>

#endif

typedef struct stat struct_stat; // avoid issue when "stat" is redirected

#include "include/libc_redirect.h"

#endif // libc_header.h
