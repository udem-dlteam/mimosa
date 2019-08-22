#ifndef _STDARG_HEADER

#define _STDARG_HEADER 1

#include "include/libc_header.h"

#define va_list const char *
#define va_arg(ap, type) (ap += sizeof(type), *(type*)(ap-sizeof(type)))
#define va_arg_char(ap) (char)va_arg(ap, int)
#define va_start(ap, last) ap = ((va_list)(last) + sizeof(*last))
#define va_end(ap)

#endif // stdarg.h
