#ifndef _MATH_HEADER

#define _MATH_HEADER 1

#include "include/libc_header.h"

extern double acos(double __x);
extern double acosh(double __x);
extern double asin(double __x);
extern double asinh(double __x);
extern double atan(double __x);
extern double atan2(double __y, double __x);
extern double atanh(double __x);
extern double ceil(double __x);
extern double cos(double __x);
extern double cosh(double __x);
extern double exp(double __x);
extern double expm1(double __x);
extern double fabs(double __x);
extern double floor(double __x);
extern double hypot(double __x, double __y);
extern int ilogb(double __x);
extern double log(double __x);
extern double log1p(double __x);
extern double modf(double __x, double *__iptr);
extern double pow(double __x, double __y);
extern double sin(double __x);
extern double sinh(double __x);
extern double sqrt(double __x);
extern double tan(double __x);
extern double tanh(double __x);
extern double scalbn(double __x, int __exp);

#ifndef USE_LIBC_LINK

extern void libc_init_math(void);

#endif

#endif // math.h
