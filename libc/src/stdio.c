#include "include/libc_common.h"
#include "include/stdio.h"
#include "include/stdarg.h"

#ifdef USE_MIMOSA

#include "../drivers/filesystem/include/vfs.h"
#include "../drivers/filesystem/include/stdstream.h"
#include "ps2.h"
#include "term.h"
#include "general.h"
#include "rtlib.h"

file* libc_stdin;

#endif

#ifndef USE_LIBC_LINK

#ifndef USE_HOST_LIBC

FILE FILE_stdin;
FILE FILE_stdout;
FILE FILE_stderr;
FILE FILE_root_dir;

#endif

#endif

FILE *REDIRECT_NAME(fopen)(const char *__restrict __filename,
                           const char *__restrict __modes) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._fopen(__filename, __modes);

#else

  libc_trace("fopen");

#ifdef USE_HOST_LIBC

  return fopen(__filename, __modes);

#else

  //  debug_write("fopen");
  //  debug_write(CAST(native_string, __filename));

  // TODO: implement
  if (__filename[0] == '.' &&
      __filename[1] == '/' &&
      __filename[2] == '\0') {

    // The file "./" is opened by Gambit by the path normalization
    // function and it must not return NULL.

    return &FILE_root_dir;
  } else {

    error_code err;
    file *f;
    if (HAS_NO_ERROR(err = file_open(CAST(native_string, __filename),
                                     CAST(native_string, __modes), &f))) {
      FILE *gambit_file = CAST(FILE *, kmalloc(sizeof(FILE)));
      gambit_file->f = f;
      return gambit_file;
    } else {
      return NULL;
    }
  }

#endif
#endif
}

FILE *REDIRECT_NAME(fdopen)(int __fd, const char *__modes) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._fdopen(__fd, __modes);

#else

  libc_trace("fdopen");

#ifdef USE_HOST_LIBC

  return fdopen(__fd, __modes);

#else

  // TODO: implement
  return NULL;

#endif
#endif
}

size_t REDIRECT_NAME(fread)(void *__restrict __ptr, size_t __size, size_t __n,
                            FILE *__restrict __stream) {
#ifdef USE_LIBC_LINK

  return LIBC_LINK._fread(__ptr, __size, __n, __stream);

#else

  libc_trace("fread");

#ifdef USE_HOST_LIBC

  return fread(__ptr, __size, __n, __stream);

#else

  error_code err;

  file *f = __stream->f;
  uint32 count = __n * __size;
  
  if (ERROR(err = file_read(f, __ptr, count))) {
    // fread interface has 0 for an error
    __n = 0;
    __stream->err = 0;
  } else {
    // Number of items read, not byte count
    __n = err / __size;
  }

  return __n;
#endif
#endif
}

size_t REDIRECT_NAME(fwrite)(const void *__restrict __ptr, size_t __size,
                             size_t __n, FILE *__restrict __stream) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._fwrite(__ptr, __size, __n, __stream);

#else

   libc_trace("fwrite");

#ifdef USE_HOST_LIBC

  return fwrite(__ptr, __size, __n, __stream);

#else

  // TODO: implement writing to files other than stdout/stderr
  if (__stream == &FILE_stdout || __stream == &FILE_stderr) {
    unicode_char *p = CAST(unicode_char*,__ptr);
    int n = __size * __n / sizeof(unicode_char);

    while (n > 0) {
      libc_wr_char(1, *p++);
      n--;
    }
  } else {
    error_code err;
    file *f = __stream->f;
    uint32 count = __size * __n;

    if (ERROR(err = file_write(f, CAST(void *, __ptr), count))) {
      __stream->err = err;
      __n = 0;
    } else {
      // No of items returned
      __n = err / __size;
    }
  }

  return __n;

#endif
#endif
}

int REDIRECT_NAME(fclose)(FILE *__restrict __stream) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._fclose(__stream);

#else

  libc_trace("fclose");

#ifdef USE_HOST_LIBC

  return fclose(__stream);

#else
  return 0;

  // Before doing actual flclose, all streams must be actually streams (STDERR and STDOUT) 

  file* sys_file = __stream->f;
  error_code err = NO_ERROR;
  if(ERROR(err = file_close(sys_file))) {
    return (-1);
  } else {
    return 0;
  }
  
#endif
#endif
}

int REDIRECT_NAME(fflush)(FILE *__restrict __stream) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._fflush(__stream);

#else

  libc_trace("fflush");

#ifdef USE_HOST_LIBC

  return fflush(__stream);

#else

  // TODO: implement
  return 0;

#endif
#endif
}

int REDIRECT_NAME(fseek)(FILE *__restrict __stream, long __off, int __whence) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._fseek(__stream, __off, __whence);

#else

  libc_trace("fseek");

#ifdef USE_HOST_LIBC

  return fseek(__stream, __off, __whence);

#else

  // TODO: implement
  return 0;

#endif
#endif
}

long REDIRECT_NAME(ftell)(FILE *__restrict __stream) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._ftell(__stream);

#else

  libc_trace("ftell");

#ifdef USE_HOST_LIBC

  return ftell(__stream);

#else

  // TODO: implement
  return 0;

#endif
#endif
}

int REDIRECT_NAME(ferror)(FILE *__restrict __stream) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._ferror(__stream);

#else

  libc_trace("ferror");

#ifdef USE_HOST_LIBC

  return ferror(__stream);

#else

  // TODO: implement
  return ERROR(__stream->err);

#endif
#endif
}

int REDIRECT_NAME(feof)(FILE *__restrict __stream) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._feof(__stream);

#else

  libc_trace("feof");

#ifdef USE_HOST_LIBC

  return feof(__stream);

#else

  return __stream->err == EOF_ERROR;

#endif
#endif
}

void REDIRECT_NAME(clearerr)(FILE *__restrict __stream) {

#ifdef USE_LIBC_LINK

  LIBC_LINK._clearerr(__stream);

#else

  libc_trace("clearerr");

#ifdef USE_HOST_LIBC

  clearerr(__stream);

#else

  __stream->err = NO_ERROR;

#endif
#endif
}

int REDIRECT_NAME(fileno)(FILE *__restrict __stream) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._fileno(__stream);

#else

  libc_trace("fileno");

#ifdef USE_HOST_LIBC

  return fileno(__stream);

#else

  // TODO: implement
  return 0;

#endif
#endif
}

void REDIRECT_NAME(setbuf )(FILE *__restrict __stream, char *__restrict __buf) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._setbuf(__stream, __buf);

#else

  libc_trace("setbuf");

#ifdef USE_HOST_LIBC

  setbuf(__stream, __buf);

#else

  // TODO: implement

#endif
#endif
}

int REDIRECT_NAME(rename)(const char *__oldpath, const char *__newpath) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._rename(__oldpath, __newpath);

#else

  libc_trace("rename");

#ifdef USE_HOST_LIBC

  return rename(__oldpath, __newpath);

#else

  return file_rename(CAST(native_string, __oldpath),
                     CAST(native_string, __newpath));

#endif
#endif
}

#ifndef USE_LIBC_LINK

typedef long longlong; // fake it to avoid 64 bit operations

int fprintf_aux_char(FILE *__restrict __stream, char __c) {
  REDIRECT_NAME(fwrite)(&__c, 1, 1, __stream);
  return 1;
}

int fprintf_aux_string(FILE *__restrict __stream, const char *__str, int __precision) {
  int n = 0;
  int len = 0;
  int i = 0;
  while (__str[len] != '\0') len++;
  while (i < len) n += fprintf_aux_char(__stream, __str[i++]);
  while (__precision > len) n += fprintf_aux_char(__stream, ' ');
  return n;
}

int fprintf_aux_longlong(FILE *__restrict __stream, longlong __num, int __base, int __precision) {

#define MAX_PRECISION 100

  char buffer[MAX_PRECISION+1];
  char *p = buffer+MAX_PRECISION;
  int neg = __num < 0;

  if (__precision > MAX_PRECISION) {
    __precision = MAX_PRECISION;
  }

  if (neg) {
    __precision--;
  } else {
    __num = -__num;
  }

  *p = '\0';

  while (__num != 0 || __precision > 0) {
    *--p = "0123456789abcdef"[-(__num % __base)];
    __num /= __base;
    __precision--;
  }

  if (neg) {
    *--p = '-';
  }

  return fprintf_aux_string(__stream, p, 0);
}

int REDIRECT_NAME(vfprintf)(FILE *__restrict __stream, const char *__format, va_list __ap) {

  int n = 0;
  char c;
  const char *fmt = __format;

  libc_trace("vfprintf");

  while ((c = *fmt++) != '\0') {
    if (c == '%') {

      int lng = 0;
      int precision = 0;
      int base = 0;
      const char *p = fmt;

      while (*p >= '0' && *p <= '9') {
        precision = precision*10 + (*p++ - '0');
      }

      if (p == fmt) {
        precision = 1;
      }

      if (*p == 'l') {
        p++;
        if (*p == 'l') {
          p++;
          lng = 2;
        } else {
          lng = 1;
        }
      }

      switch (*p) {
      case 'x':
        base += 6;
      case 'd':
      case 'u':
        base += 2;
      case 'o':
        base += 8;
        if (lng == 0) {
          n += fprintf_aux_longlong(__stream,
                                    va_arg(__ap, int), 
                                    base,
                                    precision);
        } else if (lng == 1) {
          n += fprintf_aux_longlong(__stream,
                                    va_arg(__ap, long), 
                                    base,
                                    precision);
        } else {
          n += fprintf_aux_longlong(__stream,
                                    va_arg(__ap, longlong), 
                                    base,
                                    precision);
        }
        p++;
        break;
      case 'p':
        n += fprintf_aux_longlong(__stream,
                                  (longlong)va_arg(__ap, void*), 
                                  16,
                                  precision);
        p++;
        break;
      case 's':
        n += fprintf_aux_string(__stream, va_arg(__ap, const char *), precision);
        p++;
        break;
      case 'c':
        n += fprintf_aux_char(__stream, va_arg_char(__ap));
        p++;
        break;
      case '%':
        n += fprintf_aux_char(__stream, '%');
        p++;
        break;
      }
      fmt = p;
    } else {
      n += fprintf_aux_char(__stream, c);
    }
  }

  return n;      
}

#else

int REDIRECT_NAME(vfprintf)(FILE *__restrict __stream, const char *__format, va_list __ap) {
  return LIBC_LINK._vfprintf(__stream, __format, __ap);
}

#endif

int REDIRECT_NAME(fprintf)(FILE *__restrict __stream, const char *__format, ...) {

  va_list ap;
  va_start(ap, __format);

#ifdef USE_LIBC_LINK

  return LIBC_LINK._vfprintf(__stream, __format, ap);

#else

  libc_trace("fprintf");

  return REDIRECT_NAME(vfprintf)(__stream, __format, ap);

#endif
}

int REDIRECT_NAME(printf)(const char *__format, ...) {

  va_list ap;
  va_start(ap, __format);

#ifdef USE_LIBC_LINK

  return LIBC_LINK._vfprintf(LIBC_LINK._stdout, __format, ap);

#else

  libc_trace("printf");

  return REDIRECT_NAME(vfprintf)(LIBC_LINK._stdout, __format, ap);

#endif
}

#ifndef USE_LIBC_LINK

void libc_init_stdio(void) {
#ifdef USE_HOST_LIBC
  LIBC_LINK._stdin  = stdin;
  LIBC_LINK._stdout = stdout;
  LIBC_LINK._stderr = stderr;
#else
  
  error_code err;
  // Open STDIN has a non blocking stream in readonly mode.
  // This is a "gambit" particularity...

  // Opening a stream but not reading it will lead to it being
  // full. We don't open it if we won't read it.
#ifdef GAMBIT_REPL
  if (ERROR(err = file_open(STDIN_PATH, "rx", &libc_stdin))) {
    panic(L"Failed to load LIBC stdio");
  } else {
    FILE_stdin.f = libc_stdin;
  }
#endif

  if(ERROR(err = file_open("/dsk1", "r", &FILE_root_dir.f))) {
    panic(L"Failed to load the root dir for gambit");
  }

  LIBC_LINK._stdin  = &FILE_stdin;
  LIBC_LINK._stdout = &FILE_stdout;
  LIBC_LINK._stderr = &FILE_stderr;
#endif
}

#endif
