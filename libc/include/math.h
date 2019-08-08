#ifndef _MATH_HEADER

#define _MATH_HEADER 1

#include "include/libc_header.h"

extern double REDIRECT_NAME(acos)(double __x);
extern double REDIRECT_NAME(acosh)(double __x);
extern double REDIRECT_NAME(asin)(double __x);
extern double REDIRECT_NAME(asinh)(double __x);
extern double REDIRECT_NAME(atan)(double __x);
extern double REDIRECT_NAME(atan2)(double __y, double __x);
extern double REDIRECT_NAME(atanh)(double __x);
extern double REDIRECT_NAME(ceil)(double __x);
extern double REDIRECT_NAME(cos)(double __x);
extern double REDIRECT_NAME(cosh)(double __x);
extern double REDIRECT_NAME(exp)(double __x);
extern double REDIRECT_NAME(expm1)(double __x);
extern double REDIRECT_NAME(fabs)(double __x);
extern double REDIRECT_NAME(floor)(double __x);
extern double REDIRECT_NAME(hypot)(double __x, double __y);
extern int REDIRECT_NAME(ilogb)(double __x);
extern double REDIRECT_NAME(log)(double __x);
extern double REDIRECT_NAME(log1p)(double __x);
extern double REDIRECT_NAME(modf)(double __x, double *__iptr);
extern double REDIRECT_NAME(pow)(double __x, double __y);
extern double REDIRECT_NAME(sin)(double __x);
extern double REDIRECT_NAME(sinh)(double __x);
extern double REDIRECT_NAME(sqrt)(double __x);
extern double REDIRECT_NAME(tan)(double __x);
extern double REDIRECT_NAME(tanh)(double __x);
extern double REDIRECT_NAME(scalbn)(double __x, int __exp);

#ifndef USE_LIBC_LINK

extern void libc_init_math(void);

#endif

#endif // math.h
