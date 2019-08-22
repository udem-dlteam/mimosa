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
  LIBC_LINK._opendir = REDIRECT_NAME(opendir);
  LIBC_LINK._readdir = REDIRECT_NAME(readdir);
  LIBC_LINK._closedir = REDIRECT_NAME(closedir);

  // errno.h
#if 0
  LIBC_LINK._errno = &REDIRECT_NAME(errno);
#endif

  // math.h
  LIBC_LINK._acos = REDIRECT_NAME(acos);
  LIBC_LINK._acosh = REDIRECT_NAME(acosh);
  LIBC_LINK._asin = REDIRECT_NAME(asin);
  LIBC_LINK._asinh = REDIRECT_NAME(asinh);
  LIBC_LINK._atan = REDIRECT_NAME(atan);
  LIBC_LINK._atan2 = REDIRECT_NAME(atan2);
  LIBC_LINK._atanh = REDIRECT_NAME(atanh);
  LIBC_LINK._ceil = REDIRECT_NAME(ceil);
  LIBC_LINK._cos = REDIRECT_NAME(cos);
  LIBC_LINK._cosh = REDIRECT_NAME(cosh);
  LIBC_LINK._exp = REDIRECT_NAME(exp);
  LIBC_LINK._expm1 = REDIRECT_NAME(expm1);
  LIBC_LINK._fabs = REDIRECT_NAME(fabs);
  LIBC_LINK._floor = REDIRECT_NAME(floor);
  LIBC_LINK._hypot = REDIRECT_NAME(hypot);
  LIBC_LINK._ilogb = REDIRECT_NAME(ilogb);
  LIBC_LINK._log = REDIRECT_NAME(log);
  LIBC_LINK._log1p = REDIRECT_NAME(log1p);
  LIBC_LINK._modf = REDIRECT_NAME(modf);
  LIBC_LINK._pow = REDIRECT_NAME(pow);
  LIBC_LINK._sin = REDIRECT_NAME(sin);
  LIBC_LINK._sinh = REDIRECT_NAME(sinh);
  LIBC_LINK._sqrt = REDIRECT_NAME(sqrt);
  LIBC_LINK._tan = REDIRECT_NAME(tan);
  LIBC_LINK._tanh = REDIRECT_NAME(tanh);
  LIBC_LINK._scalbn = REDIRECT_NAME(scalbn);

  // setjmp.h
  LIBC_LINK._setjmp = REDIRECT_NAME(setjmp);
  LIBC_LINK._longjmp = REDIRECT_NAME(longjmp);

  // signal.h
  LIBC_LINK._signal = REDIRECT_NAME(signal);

  // stdio.h
  LIBC_LINK._fopen = REDIRECT_NAME(fopen);
  LIBC_LINK._fdopen = REDIRECT_NAME(fdopen);
  LIBC_LINK._fread = REDIRECT_NAME(fread);
  LIBC_LINK._fwrite = REDIRECT_NAME(fwrite);
  LIBC_LINK._fclose = REDIRECT_NAME(fclose);
  LIBC_LINK._fflush = REDIRECT_NAME(fflush);
  LIBC_LINK._fseek = REDIRECT_NAME(fseek);
  LIBC_LINK._ftell = REDIRECT_NAME(ftell);
  LIBC_LINK._ferror = REDIRECT_NAME(ferror);
  LIBC_LINK._feof = REDIRECT_NAME(feof);
  LIBC_LINK._clearerr = REDIRECT_NAME(clearerr);
  LIBC_LINK._fileno = REDIRECT_NAME(fileno);
  LIBC_LINK._setbuf = REDIRECT_NAME(setbuf);
  LIBC_LINK._rename = REDIRECT_NAME(rename);
  LIBC_LINK._vfprintf = REDIRECT_NAME(vfprintf);
#if 0
  LIBC_LINK._stdin = REDIRECT_NAME(stdin);
  LIBC_LINK._stdout = REDIRECT_NAME(stdout);
  LIBC_LINK._stderr = REDIRECT_NAME(stderr);
#endif

  // stdlib.h
  LIBC_LINK._malloc = REDIRECT_NAME(malloc);
  LIBC_LINK._free = REDIRECT_NAME(free);
  LIBC_LINK._exit = REDIRECT_NAME(exit);
  LIBC_LINK._getenv = REDIRECT_NAME(getenv);
  LIBC_LINK._system = REDIRECT_NAME(system);

  // string.h
#if 0
  LIBC_LINK._memcpy = REDIRECT_NAME(memcpy);
  LIBC_LINK._memmove = REDIRECT_NAME(memmove);
#endif

  // termios.h
  LIBC_LINK._tcgetattr = REDIRECT_NAME(tcgetattr);
  LIBC_LINK._tcsetattr = REDIRECT_NAME(tcsetattr);
  LIBC_LINK._cfsetospeed = REDIRECT_NAME(cfsetospeed);
  LIBC_LINK._cfsetispeed = REDIRECT_NAME(cfsetispeed);

  // time.h
  LIBC_LINK._clock = REDIRECT_NAME(clock);
  LIBC_LINK._nanosleep = REDIRECT_NAME(nanosleep);
  LIBC_LINK._clock_getres = REDIRECT_NAME(clock_getres);
  LIBC_LINK._clock_gettime = REDIRECT_NAME(clock_gettime);
  LIBC_LINK._clock_settime = REDIRECT_NAME(clock_settime);

  // unistd.h
  LIBC_LINK._chdir = REDIRECT_NAME(chdir);
  LIBC_LINK._getcwd = REDIRECT_NAME(getcwd);
  LIBC_LINK._mkdir = REDIRECT_NAME(mkdir);
  LIBC_LINK._remove = REDIRECT_NAME(remove);
  LIBC_LINK._stat = REDIRECT_NAME(stat);
  LIBC_LINK._lstat = REDIRECT_NAME(lstat);
  LIBC_LINK._isatty = REDIRECT_NAME(isatty);

  // sys/resource.h
  LIBC_LINK._getrusage = REDIRECT_NAME(getrusage);

  // sys/time.h
  LIBC_LINK._gettimeofday = REDIRECT_NAME(gettimeofday);
  LIBC_LINK._settimeofday = REDIRECT_NAME(settimeofday);
  LIBC_LINK._getitimer = REDIRECT_NAME(getitimer);
  LIBC_LINK._setitimer = REDIRECT_NAME(setitimer);

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
