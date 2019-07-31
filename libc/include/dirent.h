#ifndef _DIRENT_HEADER

#define _DIRENT_HEADER 1

#include "include/libc_header.h"

#ifdef USE_MIMOSA
#include "fs.h"
#else
#ifndef USE_HOST_LIBC

typedef struct {
  int state;
} DIR;

struct dirent {
  char d_name[256];  // Null-terminated filename
};

#endif
extern DIR *opendir(char *__name);
extern struct dirent *readdir(DIR *__dirp);
extern int closedir(DIR *__dirp);
#endif

#ifndef USE_LIBC_LINK

extern void libc_init_dirent(void);

#endif

#endif // dirent.h
