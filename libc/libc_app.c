#include "include/libc_common.h"

extern int main(int argc, char *argv[], char *env[]);

int libc_startup(int argc, char *argv[], char *env[]) {
  return main(argc, argv, env);
}

#ifndef USE_MIMOSA_LIBC_LINK
struct libc_link LIBC_LINK;
#endif

#include "src/dirent.c"
#include "src/errno.c"
#include "src/math.c"
#include "src/signal.c"
#include "src/setjmp.c"
#include "src/stdio.c"
#include "src/stdlib.c"
#include "src/string.c"
#include "src/termios.c"
#include "src/time.c"
#include "src/unistd.c"
#include "src/sys_time.c"
#include "src/sys_resource.c"
