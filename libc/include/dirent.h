#ifndef _DIRENT_HEADER

#define _DIRENT_HEADER 1

#include "include/libc_header.h"

#ifdef USE_MIMOSA

#include "../drivers/filesystem/include/vfs.h"

#else

#ifndef USE_HOST_LIBC

typedef struct {
  int state;
} DIR;

typedef struct dirent {
  char d_name[256];  // Null-terminated filename
} dirent;

#endif

#endif

extern DIR *REDIRECT_NAME(opendir)(const char *__name);
extern dirent *REDIRECT_NAME(readdir)(DIR *__dirp);
extern int REDIRECT_NAME(closedir)(DIR *__dirp);

#ifndef USE_LIBC_LINK

extern void libc_init_dirent(void);

#endif

#endif // dirent.h
