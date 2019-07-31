#ifndef _TERMIOS_HEADER

#define _TERMIOS_HEADER 1

#include "include/libc_header.h"

#ifndef USE_HOST_LIBC

typedef unsigned char cc_t;
typedef unsigned int  speed_t;
typedef unsigned int  tcflag_t;

#define NCCS 32
struct termios {
  tcflag_t c_iflag; // input mode flags
  tcflag_t c_oflag; // output mode flags
  tcflag_t c_cflag; // control mode flags
  tcflag_t c_lflag; // local mode flags
  cc_t c_line;      // line discipline
  cc_t c_cc[NCCS];  // control characters
  speed_t c_ispeed; // input speed
  speed_t c_ospeed; // output speed
};

// c_cc characters
#define VINTR    0
#define VQUIT    1
#define VERASE   2
#define VKILL    3
#define VEOF     4
#define VTIME    5
#define VMIN     6
#define VSWTC    7
#define VSTART   8
#define VSTOP    9
#define VSUSP    10
#define VEOL     11
#define VREPRINT 12
#define VDISCARD 13
#define VWERASE  14
#define VLNEXT   15
#define VEOL2    16

// c_iflag bits
#define IGNBRK  0000001
#define BRKINT  0000002
#define IGNPAR  0000004
#define PARMRK  0000010
#define INPCK   0000020
#define ISTRIP  0000040
#define INLCR   0000100
#define IGNCR   0000200
#define ICRNL   0000400
#define IUCLC   0001000
#define IXON    0002000
#define IXANY   0004000
#define IXOFF   0010000
#define IMAXBEL 0020000
#define IUTF8   0040000

// c_oflag bits
#define OPOST   0000001
#define OLCUC   0000002
#define ONLCR   0000004
#define OCRNL   0000010
#define ONOCR   0000020
#define ONLRET  0000040
#define OFILL   0000100
#define OFDEL   0000200

// c_cflag bit meaning
#define B0     0000000
#define B50    0000001
#define B75    0000002
#define B110   0000003
#define B134   0000004
#define B150   0000005
#define B200   0000006
#define B300   0000007
#define B600   0000010
#define B1200  0000011
#define B1800  0000012
#define B2400  0000013
#define B4800  0000014
#define B9600  0000015
#define B19200 0000016
#define B38400 0000017
#define CSIZE  0000060
#define CS5    0000000
#define CS6    0000020
#define CS7    0000040
#define CS8    0000060
#define CSTOPB 0000100
#define CREAD  0000200
#define PARENB 0000400
#define PARODD 0001000
#define HUPCL  0002000
#define CLOCAL 0004000

// c_lflag bits
#define ISIG    0000001
#define ICANON  0000002
#define ECHO    0000010
#define ECHOE   0000020
#define ECHOK   0000040
#define ECHONL  0000100
#define NOFLSH  0000200
#define TOSTOP  0000400
#define ECHOCTL 0001000
#define ECHOPRT 0002000
#define ECHOKE  0004000
#define FLUSHO  0010000
#define PENDIN  0040000
#define IEXTEN  0100000

#define TCSANOW   0
#define TCSADRAIN 1
#define TCSAFLUSH 2

#endif

extern int tcgetattr(int __fd, struct termios *__termios_p);

extern int tcsetattr(int __fd, int __optional_actions,
                     const struct termios *__termios_p);

extern int cfsetospeed(struct termios *__termios_p, speed_t __speed);

extern int cfsetispeed(struct termios *__termios_p, speed_t __speed);

#ifndef USE_LIBC_LINK

extern void libc_init_termios(void);

#endif

#endif // termios.h
