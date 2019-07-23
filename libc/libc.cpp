#define USE_MIMOSA
#undef REDIRECT_PREFIX
#define REDIRECT_PREFIX libc_

#include "include/libc_link.h"

#include "src/libc_support.c"

#include "include/dirent.h"
#include "include/errno.h"
#include "include/math.h"
#include "include/setjmp.h"
#include "include/stdio.h"
#include "include/stdlib.h"
#include "include/string.h"
#include "include/time.h"
#include "include/unistd.h"

#include "src/libc_link.c"

#include "src/dirent.c"
#include "src/errno.c"
#include "src/math.c"
#include "src/setjmp.c"
#include "src/stdio.c"
#include "src/stdlib.c"
#include "src/string.c"
#include "src/time.c"
#include "src/unistd.c"
