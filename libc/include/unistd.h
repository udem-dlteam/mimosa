#ifndef _UNISTD_HEADER

#define _UNISTD_HEADER 1

#include "include/libc_header.h"
#include "include/stddef.h"
#include "include/time.h"

#ifdef USE_MIMOSA

#include "../drivers/filesystem/include/vfs.h"

#else

#ifndef USE_HOST_LIBC

typedef uint32 __dev_t;
typedef uint32 __ino_t;
typedef uint32 __mode_t;
typedef uint32 __nlink_t;
typedef uint32 __uid_t;
typedef uint32 __gid_t;
typedef uint32 __off_t;

struct stat {
  __dev_t st_dev;      // Device
  __ino_t st_ino;      // File serial number
  __mode_t st_mode;    // File mode
  __nlink_t st_nlink;  // Link count
  __uid_t st_uid;      // User ID of the file's owner
  __gid_t st_gid;      // Group ID of the file's group
  __off_t st_size;     // Size of file, in bytes

  struct timespec st_atim;  // Time of last access
  struct timespec st_mtim;  // Time of last modification
  struct timespec st_ctim;  // Time of last status change

#define st_atime st_atim.ts_sec
#define st_mtime st_mtim.ts_sec
#define st_ctime st_ctim.ts_sec
};

#define S_IFMT   0170000
#define S_IFSOCK 0140000
#define S_IFLNK  0120000
#define S_IFREG  0100000
#define S_IFBLK  0060000
#define S_IFDIR  0040000
#define S_IFCHR  0020000
#define S_IFIFO  0010000
#define S_ISUID  0004000
#define S_ISGID  0002000
#define S_ISVTX  0001000

#define S_IRUSR 4               // Read by owner
#define S_IWUSR 2               // Write by owner
#define S_IXUSR 1               // Execute by owner
#define S_IRWXU 7

#define S_IRGRP (S_IRUSR >> 3)  // Read by group
#define S_IWGRP (S_IWUSR >> 3)  // Write by group
#define S_IXGRP (S_IXUSR >> 3)  // Execute by group
#define S_IRWXG (S_IRWXU >> 3)

#define S_IROTH (S_IRGRP >> 3)  // Read by others
#define S_IWOTH (S_IWGRP >> 3)  // Write by others
#define S_IXOTH (S_IXGRP >> 3)  // Execute by others
#define S_IRWXO (S_IRWXG >> 3)

#define S_IREAD  S_IRUSR
#define S_IWRITE S_IWUSR
#define S_IEXEC  S_IXUSR

#define S_ISLNK(m)  (((m) & S_IFMT) == S_IFLNK)
#define S_ISREG(m)  (((m) & S_IFMT) == S_IFREG)
#define S_ISDIR(m)  (((m) & S_IFMT) == S_IFDIR)
#define S_ISCHR(m)  (((m) & S_IFMT) == S_IFCHR)
#define S_ISBLK(m)  (((m) & S_IFMT) == S_IFBLK)
#define S_ISFIFO(m) (((m) & S_IFMT) == S_IFIFO)
#define S_ISSOCK(m) (((m) & S_IFMT) == S_IFSOCK)

#endif
#endif

extern int REDIRECT_NAME(chdir)(const char *__path);
extern char *REDIRECT_NAME(getcwd)(char *__buf, size_t __size);
extern int REDIRECT_NAME(mkdir)(const char *__pathname, mode_t __mode);
extern int REDIRECT_NAME(remove)(const char *__pathname);
extern int REDIRECT_NAME(lstat)(const char *__pathname, struct stat *__buf);
extern int REDIRECT_NAME(stat)(const char *__pathname, struct stat *__buf);
extern int REDIRECT_NAME(isatty)(int __fd);

#ifndef USE_LIBC_LINK

extern void libc_init_unistd(void);

#endif

#endif // unistd.h
