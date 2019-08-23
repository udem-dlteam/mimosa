#include "include/libc_common.h"

#ifdef USE_MIMOSA

#include "term.h"
#include "ps2.h"

void libc_wr_char(int fd, char c) {
  // write(fd, &c, 1);
  term_write(&term_console, (native_char) c);
}

void libc_wr_string(int fd, const char *s) {
  // TODO: The file descriptor must be checked
  const char *p = s;
  while (*p != '\0') {
    libc_wr_char(fd, *p++);
  }
}

int libc_rd_char(int fd) {
  return readline();
}

#else

#ifndef USE_HOST_LIBC

// normally in unistd.h
typedef int ssize_t;
ssize_t write(int fd, const void *buf, size_t count);
ssize_t read(int fd, void *buf, size_t count);

void libc_wr_char(int fd, char c) {
  write(fd, &c, 1);
}

void libc_wr_string(int fd, const char *s) {
  const char *p = s;
  while (*p != '\0') {
    libc_wr_char(fd, *p++);
  }
}

int libc_rd_char(int fd) {
  char c;
  if (read(fd, &c, 1) == 1)
    return c;
  return -1;
}

#endif
#endif

#ifdef ENABLE_LIBC_TRACE

void libc_trace_(const char *msg) {

#ifdef USE_HOST_LIBC

  printf("------> %s\n", msg);

#else

  term_write(cout, CAST(native_string, msg));
  term_writeline(cout);

#endif
}

#endif

#ifndef USE_MIMOSA
struct libc_link LIBC_LINK;
#endif
