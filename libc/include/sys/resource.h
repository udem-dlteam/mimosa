#ifndef _SYS_RESOURCE_HEADER

#define _SYS_RESOURCE_HEADER 1

#include "include/libc_header.h"
#include "include/sys/time.h"

#ifndef USE_HOST_LIBC

#define RUSAGE_SELF 0

struct rusage {
  struct timeval ru_utime;
  struct timeval ru_stime;
  size_t ru_minflt;
  size_t ru_majflt;
};

#endif

extern int getrusage(int __who, struct rusage *__usage);

#ifndef USE_LIBC_LINK

extern void libc_init_sys_resource(void);

#endif

#endif // sys/resource.h


