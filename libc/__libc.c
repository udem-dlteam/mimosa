#include "include/libc_common.h"

extern int main(int argc, char *argv[], char *env[]);

int libc_startup(int argc, char *argv[], char *env[]) {
#ifndef USE_MIMOSA
  libc_init();
#endif
  return main(argc, argv, env);
}

#ifndef USE_MIMOSA
struct libc_link LIBC_LINK;
#endif

#include "src/dirent.c"
#include "src/errno.c"
#include "src/math.c"
#include "src/setjmp.c"
#include "src/stdio.c"
#include "src/stdlib.c"
#include "src/string.c"
#include "src/time.c"
#include "src/unistd.c"
