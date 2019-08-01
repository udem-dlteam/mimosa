#include "include/libc_link.h"
#include "include/dirent.h"
#include "include/errno.h"
#include "include/math.h"
#include "include/setjmp.h"
#include "include/signal.h"
#include "include/stdio.h"
#include "include/stdlib.h"
#include "include/string.h"
#include "include/termios.h"
#include "include/time.h"
#include "include/unistd.h"
#include "include/sys/time.h"
#include "include/sys/resource.h"

void libc_init(void) {

  libc_trace("libc_init begin");

  // dirent.h
  LIBC_LINK._opendir = opendir;
  LIBC_LINK._readdir = readdir;
  LIBC_LINK._closedir = closedir;

  // errno.h
#if 0
  LIBC_LINK._errno = &errno;
#endif

  // math.h
  LIBC_LINK._acos = acos;
  LIBC_LINK._acosh = acosh;
  LIBC_LINK._asin = asin;
  LIBC_LINK._asinh = asinh;
  LIBC_LINK._atan = atan;
  LIBC_LINK._atan2 = atan2;
  LIBC_LINK._atanh = atanh;
  LIBC_LINK._ceil = ceil;
  LIBC_LINK._cos = cos;
  LIBC_LINK._cosh = cosh;
  LIBC_LINK._exp = exp;
  LIBC_LINK._expm1 = expm1;
  LIBC_LINK._fabs = fabs;
  LIBC_LINK._floor = floor;
  LIBC_LINK._hypot = hypot;
  LIBC_LINK._ilogb = ilogb;
  LIBC_LINK._log = log;
  LIBC_LINK._log1p = log1p;
  LIBC_LINK._modf = modf;
  LIBC_LINK._pow = pow;
  LIBC_LINK._sin = sin;
  LIBC_LINK._sinh = sinh;
  LIBC_LINK._sqrt = sqrt;
  LIBC_LINK._tan = tan;
  LIBC_LINK._tanh = tanh;
  LIBC_LINK._scalbn = scalbn;

  // setjmp.h
  LIBC_LINK._setjmp = setjmp;
  LIBC_LINK._longjmp = longjmp;

  // signal.h
  LIBC_LINK._signal = signal;

  // stdio.h
  LIBC_LINK._fopen = fopen;
  LIBC_LINK._fdopen = fdopen;
  LIBC_LINK._fread = fread;
  LIBC_LINK._fwrite = fwrite;
  LIBC_LINK._fclose = fclose;
  LIBC_LINK._fflush = fflush;
  LIBC_LINK._fseek = fseek;
  LIBC_LINK._ftell = ftell;
  LIBC_LINK._ferror = ferror;
  LIBC_LINK._feof = feof;
  LIBC_LINK._clearerr = clearerr;
  LIBC_LINK._fileno = fileno;
  LIBC_LINK._setbuf = setbuf;
  LIBC_LINK._rename = rename;
  LIBC_LINK._fprintf_aux = fprintf_aux;
#if 0
  LIBC_LINK._stdin = stdin;
  LIBC_LINK._stdout = stdout;
  LIBC_LINK._stderr = stderr;
#endif

  // stdlib.h
  LIBC_LINK._malloc = malloc;
  LIBC_LINK._free = free;
  LIBC_LINK._exit = exit;
  LIBC_LINK._getenv = getenv;
  LIBC_LINK._system = system;

  // string.h
#if 0
  LIBC_LINK._memcpy = memcpy;
  LIBC_LINK._memmove = memmove;
#endif

  // termios.h
  LIBC_LINK._tcgetattr = tcgetattr;
  LIBC_LINK._tcsetattr = tcsetattr;
  LIBC_LINK._cfsetospeed = cfsetospeed;
  LIBC_LINK._cfsetispeed = cfsetispeed;

  // time.h
  LIBC_LINK._clock = clock;
  LIBC_LINK._nanosleep = nanosleep;
  LIBC_LINK._clock_getres = clock_getres;
  LIBC_LINK._clock_gettime = clock_gettime;
  LIBC_LINK._clock_settime = clock_settime;

  // unistd.h
  LIBC_LINK._getcwd = getcwd;
  LIBC_LINK._mkdir = mkdir;
  LIBC_LINK._remove = remove;
  LIBC_LINK._stat = stat;
  LIBC_LINK._lstat = lstat;
  LIBC_LINK._isatty = isatty;

  // sys/resource.h
  LIBC_LINK._getrusage = getrusage;

  // sys/time.h
  LIBC_LINK._gettimeofday = gettimeofday;
  LIBC_LINK._settimeofday = settimeofday;
  LIBC_LINK._getitimer = getitimer;
  LIBC_LINK._setitimer = setitimer;

  libc_init_dirent();
  libc_init_errno();
  libc_init_math();
  libc_init_setjmp();
  libc_init_signal();
  libc_init_stdio();
  libc_init_stdlib();
  libc_init_string();
  libc_init_termios();
  libc_init_time();
  libc_init_unistd();
  libc_init_sys_time();
  libc_init_sys_resource();

  libc_trace("libc_init end");

}
