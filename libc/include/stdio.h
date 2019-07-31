#ifndef _STDIO_HEADER

#define _STDIO_HEADER 1

#include "include/libc_header.h"
#include "include/stddef.h"

#ifdef USE_MIMOSA
#include "fs.h"
#endif

#ifndef USE_HOST_LIBC

typedef struct {
  file* f;
  error_code err;
} FILE;

#endif

extern FILE *fopen(const char *__restrict __filename,
                   const char *__restrict __modes);

extern FILE *fdopen(int __fd, const char *__modes);

extern size_t fread(void *__restrict __ptr, size_t __size,
                    size_t __n, FILE *__restrict __stream);

extern size_t fwrite(const void *__restrict __ptr, size_t __size,
                     size_t __n, FILE *__restrict __stream);

extern int fclose(FILE *__restrict __stream);

extern int fflush(FILE *__restrict __stream);

extern int fseek(FILE *__restrict __stream, long __off, int __whence);

extern long ftell(FILE *__restrict __stream);

extern int ferror(FILE *__restrict __stream);

extern int feof(FILE *__restrict __stream);

extern void clearerr(FILE *__restrict __stream);

extern int fileno(FILE *__restrict __stream);

extern void setbuf(FILE *__restrict __stream, char *__restrict __buf);

extern int rename(const char *__oldpath, const char *__newpath);

extern int fprintf_aux(FILE *__restrict __stream, const char **__format);

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
