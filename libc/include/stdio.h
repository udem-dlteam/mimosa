#ifndef _STDIO_HEADER

#define _STDIO_HEADER 1

#include "include/libc_header.h"
#include "include/stddef.h"

#ifdef USE_MIMOSA
#include "general.h"
#include "../drivers/filesystem/include/vfs.h"
#endif

#ifndef USE_HOST_LIBC

typedef struct {
  //  int state;
  file* f;
  error_code err;
} FILE;

#endif

extern FILE *REDIRECT_NAME(fopen)(const char *__restrict __filename,
                                  const char *__restrict __modes);

extern FILE *REDIRECT_NAME(fdopen)(int __fd, const char *__modes);

extern size_t REDIRECT_NAME(fread)(void *__restrict __ptr, size_t __size,
                                   size_t __n, FILE *__restrict __stream);

extern size_t REDIRECT_NAME(fwrite)(const void *__restrict __ptr, size_t __size,
                                    size_t __n, FILE *__restrict __stream);

extern int REDIRECT_NAME(fclose)(FILE *__restrict __stream);

extern int REDIRECT_NAME(fflush)(FILE *__restrict __stream);

extern int REDIRECT_NAME(fseek)(FILE *__restrict __stream, long __off, int __whence);

extern long REDIRECT_NAME(ftell)(FILE *__restrict __stream);

extern int REDIRECT_NAME(ferror)(FILE *__restrict __stream);

extern int REDIRECT_NAME(feof)(FILE *__restrict __stream);

extern void REDIRECT_NAME(clearerr)(FILE *__restrict __stream);

extern int REDIRECT_NAME(fileno)(FILE *__restrict __stream);

extern void REDIRECT_NAME(setbuf)(FILE *__restrict __stream, char *__restrict __buf);

extern int REDIRECT_NAME(rename)(const char *__oldpath, const char *__newpath);

extern int REDIRECT_NAME(fprintf_aux)(FILE *__restrict __stream, const char **__format);

#ifdef USE_LIBC_LINK

#define stdin  LIBC_LINK._stdin
#define stdout LIBC_LINK._stdout
#define stderr LIBC_LINK._stderr

#else

#ifndef USE_HOST_LIBC

extern FILE FILE_stdin;
extern FILE FILE_stdout;
extern FILE FILE_stderr;

#endif

extern void libc_init_stdio(void);

#endif

#endif // stdio.h
