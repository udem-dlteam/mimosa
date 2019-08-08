#include "include/libc_common.h"
#include "include/termios.h"

int REDIRECT_NAME(tcgetattr)(int __fd, struct termios *__termios_p) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._tcgetattr(__fd, __termios_p);

#else

  libc_trace("tcgetattr");

#ifdef USE_HOST_LIBC

  return tcgetattr(__fd, __termios_p);

#else

  // TODO: implement
  return 0;

#endif
#endif
}

int REDIRECT_NAME(tcsetattr)(int __fd, int __optional_actions,
                             const struct termios *__termios_p) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._tcsetattr(__fd, __optional_actions, __termios_p);

#else

  libc_trace("tcsetattr");

#ifdef USE_HOST_LIBC

  return tcsetattr(__fd, __optional_actions, __termios_p);

#else

  // TODO: implement
  return 0;

#endif
#endif
}

int REDIRECT_NAME(cfsetospeed)(struct termios *__termios_p, speed_t __speed) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._cfsetospeed(__termios_p, __speed);

#else

  libc_trace("cfsetospeed");

#ifdef USE_HOST_LIBC

  return cfsetospeed(__termios_p, __speed);

#else

  // TODO: implement
  return 0;

#endif
#endif
}

int REDIRECT_NAME(cfsetispeed)(struct termios *__termios_p, speed_t __speed) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._cfsetispeed(__termios_p, __speed);

#else

  libc_trace("cfsetispeed");

#ifdef USE_HOST_LIBC

  return cfsetispeed(__termios_p, __speed);

#else

  // TODO: implement
  return 0;

#endif
#endif
}

#ifndef USE_LIBC_LINK

void libc_init_termios(void) {
}

#endif
