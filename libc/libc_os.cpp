//#define ENABLE_LIBC_TRACE
//#define USE_MIMOSA
#define USE_MIMOSA_LIBC_LINK
#undef REDIRECT_PREFIX
#define REDIRECT_PREFIX libc_

#ifdef USE_MIMOSA

#include "general.h"
#include "chrono.h"
#include "ps2.h"

// #include <math.h>

#endif

#include "include/libc_link.h"

#include "src/libc_support.c"

#include "include/dirent.h"
#include "include/errno.h"
#include "include/math.h"
#include "include/setjmp.h"
#include "include/signal.h"
#include "include/stdio.h"
#include "include/stdlib.h"
#include "include/string.h"
#include "include/termios.h"
#include "include/time.h"
#include "include/unistd.h"
#include "include/sys/time.h"
#include "include/sys/resource.h"

#include "src/libc_link.c"

#include "src/dirent.c"
#include "src/errno.c"
#include "src/math.c"
#include "src/setjmp.c"
#include "src/signal.c"
#include "src/stdio.c"
#include "src/stdlib.c"
#include "src/string.c"
#include "src/termios.c"
#include "src/time.c"
#include "src/unistd.c"
#include "src/sys_time.c"
#include "src/sys_resource.c"
