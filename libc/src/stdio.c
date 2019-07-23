#include "include/libc_common.h"
#include "include/stdio.h"

#ifndef USE_LIBC_LINK

#ifndef USE_HOST_LIBC

FILE FILE_stdin;
FILE FILE_stdout;
FILE FILE_stderr;
FILE FILE_root_dir;

#endif

#endif

FILE *fopen(const char *__restrict __filename,
            const char *__restrict __modes) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._fopen(__filename, __modes);

#else

  libc_trace("fopen");

#ifdef USE_HOST_LIBC

#undef fopen

  return fopen(__filename, __modes);

#else

  /* TODO: implement */
  if (__filename[0] == '.' &&
      __filename[1] == '/' &&
      __filename[2] == '\0') {
    /*
     * The file "./" is opened by Gambit by the path normalization
     * function and it must not return NULL.
     */
    return &FILE_root_dir;
  } else {
    /*
     * It is OK to return NULL for other files (of course this means
     * files can't be accessed).
     */
    return NULL;
  }

#endif
#endif
}

FILE *fdopen(int __fd, const char *__modes) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._fdopen(__fd, __modes);

#else

  libc_trace("fdopen");

#ifdef USE_HOST_LIBC

#undef fdopen

  return fdopen(__fd, __modes);

#else

  /* TODO: implement */
  return NULL;

#endif
#endif
}

size_t fread(void *__restrict __ptr, size_t __size,
             size_t __n, FILE *__restrict __stream) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._fread(__ptr, __size, __n, __stream);

#else

  libc_trace("fread");

#ifdef USE_HOST_LIBC

#undef fread

  return fread(__ptr, __size, __n, __stream);

#else

  /* TODO: implement reading other files than stdin */

  if (__stream == &FILE_stdin) {

    char *p = (char*)__ptr;
    int n = __size * __n;

    while (n > 0) {
      *p++ = libc_rd_char(0);
      n--;
    }
  }

  return __n;

#endif
#endif
}

size_t fwrite(const void *__restrict __ptr, size_t __size,
              size_t __n, FILE *__restrict __stream) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._fwrite(__ptr, __size, __n, __stream);

#else

  //  libc_trace("fwrite");

#ifdef USE_HOST_LIBC

#undef fwrite

  return fwrite(__ptr, __size, __n, __stream);

#else

  /* TODO: implement writing to files other than stdout/stderr */

  if (__stream == &FILE_stdout || __stream == &FILE_stderr) {

    char *p = (char*)__ptr;
    int n = __size * __n;

    while (n > 0) {
      libc_wr_char(1, *p++);
      n--;
    }
  }

  return __n;

#endif
#endif
}

int fclose(FILE *__stream) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._fclose(__stream);

#else

  libc_trace("fclose");

#ifdef USE_HOST_LIBC

#undef fclose

  return fclose(__stream);

#else

  /* TODO: implement */
  return 0;

#endif
#endif
}

int fflush(FILE *__stream) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._fflush(__stream);

#else

  libc_trace("fflush");

#ifdef USE_HOST_LIBC

#undef fflush

  return fflush(__stream);

#else

  /* TODO: implement */
  return 0;

#endif
#endif
}

int fseek(FILE *__stream, long __off, int __whence) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._fseek(__stream, __off, __whence);

#else

  libc_trace("fseek");

#ifdef USE_HOST_LIBC

#undef fseek

  return fseek(__stream, __off, __whence);

#else

  /* TODO: implement */
  return 0;

#endif
#endif
}

long ftell(FILE *__stream) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._ftell(__stream);

#else

  libc_trace("ftell");

#ifdef USE_HOST_LIBC

#undef ftell

  return ftell(__stream);

#else

  /* TODO: implement */
  return 0;

#endif
#endif
}

void clearerr(FILE *__stream) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._clearerr(__stream);

#else

  libc_trace("clearerr");

#ifdef USE_HOST_LIBC

#undef clearerr

  clearerr(__stream);

#else

  /* TODO: implement */

#endif
#endif
}

int ferror(FILE *__stream) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._ferror(__stream);

#else

  libc_trace("ferror");

#ifdef USE_HOST_LIBC

#undef ferror

  return ferror(__stream);

#else

  /* TODO: implement */
  return 0;

#endif
#endif
}

int rename(const char *__oldpath, const char *__newpath) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._rename(__oldpath, __newpath);

#else

  libc_trace("rename");

#ifdef USE_HOST_LIBC

#undef rename

  return rename(__oldpath, __newpath);

#else

  /* TODO: implement */
  return 0;

#endif
#endif
}

#ifndef USE_LIBC_LINK

typedef long longlong; /* fake it to avoid 64 bit operations */

int fprintf_aux_char(FILE *__stream, char __c) {
  fwrite(&__c, 1, 1, __stream);
  return 1;
}

int fprintf_aux_string(FILE *__stream, const char *__str, int __precision) {
  int n = 0;
  int len = 0;
  int i = 0;
  while (__str[len] != '\0') len++;
  while (i < len) n += fprintf_aux_char(__stream, __str[i++]);
  while (__precision > len) n += fprintf_aux_char(__stream, ' ');
  return n;
}

int fprintf_aux_longlong(FILE *__stream, longlong __num, int __base, int __precision) {

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

int fprintf_aux(FILE *__stream, const char **__format) {

#define VA_LIST const char *
#define VA_ARG(ap, type) (ap += sizeof(type), *(type*)(ap-sizeof(type)))
#define VA_ARG_CHAR(ap) (char)VA_ARG(ap, int)
#define VA_START(ap, last) ap = ((VA_LIST)(last) + sizeof(*last))

  int n = 0;
  char c;
  VA_LIST ap;
  const char *fmt = *__format;

  VA_START(ap, __format);

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
                                    VA_ARG(ap, int), 
                                    base,
                                    precision);
        } else if (lng == 1) {
          n += fprintf_aux_longlong(__stream,
                                    VA_ARG(ap, long), 
                                    base,
                                    precision);
        } else {
          n += fprintf_aux_longlong(__stream,
                                    VA_ARG(ap, longlong), 
                                    base,
                                    precision);
        }
        p++;
        break;
      case 'p':
        n += fprintf_aux_longlong(__stream,
                                  (longlong)VA_ARG(ap, void*), 
                                  16,
                                  precision);
        p++;
        break;
      case 's':
        n += fprintf_aux_string(__stream, VA_ARG(ap, const char *), precision);
        p++;
        break;
      case 'c':
        n += fprintf_aux_char(__stream, VA_ARG_CHAR(ap));
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

#endif

int fprintf(FILE *__stream, const char *__format, ...) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._fprintf_aux(__stream, &__format);

#else

  libc_trace("fprintf");

  return fprintf_aux(__stream, &__format);

#endif
}

int printf(const char *__format, ...) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._fprintf_aux(LIBC_LINK._stdout, &__format);

#else

  libc_trace("printf");

  return fprintf_aux(LIBC_LINK._stdout, &__format);

#endif
}

#ifndef USE_LIBC_LINK

void libc_init_stdio(void) {
#ifdef USE_HOST_LIBC
  LIBC_LINK._stdin  = stdin;
  LIBC_LINK._stdout = stdout;
  LIBC_LINK._stderr = stderr;
#else
  LIBC_LINK._stdin  = &FILE_stdin;
  LIBC_LINK._stdout = &FILE_stdout;
  LIBC_LINK._stderr = &FILE_stderr;
#endif
}

#endif
