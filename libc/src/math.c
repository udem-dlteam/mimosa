#include "include/libc_common.h"
#include "include/math.h"

double REDIRECT_NAME(acos)(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._acos(__x);

#else

  libc_trace("acos");debug_write("acos");

#ifdef USE_HOST_LIBC

  return acos(__x);

#else

  return acos(__x);

#endif
#endif
}

double REDIRECT_NAME(acosh)(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._acosh(__x);

#else

  libc_trace("acosh");debug_write("acosh");

#ifdef USE_HOST_LIBC

  return acosh(__x);

#else

  // TODO: implement
  return 0.0;

#endif
#endif
}

double REDIRECT_NAME(asin)(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._asin(__x);

#else

  libc_trace("asin");debug_write("asin");

#ifdef USE_HOST_LIBC

  return asin(__x);

#else

  return asin(__x);

#endif
#endif
}

double REDIRECT_NAME(asinh)(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._asinh(__x);

#else

  libc_trace("asinh");debug_write("asinh");

#ifdef USE_HOST_LIBC

  return asinh(__x);

#else

  // TODO: implement
  return 0.0;

#endif
#endif
}

double REDIRECT_NAME(atan)(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._atan(__x);

#else

  libc_trace("atan");debug_write("atan");

#ifdef USE_HOST_LIBC

  return atan(__x);

#else

  // TODO: implement
  return 0.0;

#endif
#endif
}

double REDIRECT_NAME(atan2)(double __y, double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._atan2(__y, __x);

#else

  libc_trace("atan2");debug_write("atan2");

#ifdef USE_HOST_LIBC

  return atan2(__y, __x);

#else

  // TODO: implement
  return 0.0;

#endif
#endif
}

double REDIRECT_NAME(atanh)(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._atanh(__x);

#else

  libc_trace("atanh");debug_write("atanh");

#ifdef USE_HOST_LIBC

  return atanh(__x);

#else

  // TODO: implement
  return 0.0;

#endif
#endif
}

double REDIRECT_NAME(ceil)(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._ceil(__x);

#else

  libc_trace("ceil");

#ifdef USE_HOST_LIBC

  return ceil(__x);

#else

  return ceil(__x);

#endif
#endif
}

double REDIRECT_NAME(cos)(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._cos(__x);

#else

  libc_trace("cos");debug_write("cos");

#ifdef USE_HOST_LIBC

  return cos(__x);

#else

  // TODO: implement
  return 0.0;

#endif
#endif
}

double REDIRECT_NAME(cosh)(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._cosh(__x);

#else

  libc_trace("cosh");debug_write("cosh");

#ifdef USE_HOST_LIBC

  return cosh(__x);

#else

  // TODO: implement
  return 0.0;

#endif
#endif
}

double REDIRECT_NAME(expm1)(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._expm1(__x);

#else

  libc_trace("expm1");debug_write("expm1");

#ifdef USE_HOST_LIBC

  return expm1(__x);

#else

  return expm1(__x);

#endif
#endif
}

double REDIRECT_NAME(exp)(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._exp(__x);

#else

  libc_trace("exp");debug_write("exp");

#ifdef USE_HOST_LIBC

  return exp(__x);

#else

  return expm1(__x) + 1.0;

#endif
#endif
}

double REDIRECT_NAME(fabs)(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._fabs(__x);

#else

  libc_trace("fabs");

#ifdef USE_HOST_LIBC

  return fabs(__x);

#else

  return fabs(__x);

#endif
#endif
}

double REDIRECT_NAME(floor)(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._floor(__x);

#else

  libc_trace("floor");

#ifdef USE_HOST_LIBC

  return floor(__x);

#else

  return floor(__x);

#endif
#endif
}

double REDIRECT_NAME(hypot)(double __x, double __y) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._hypot(__x, __y);

#else

  libc_trace("hypot");debug_write("hypot");

#ifdef USE_HOST_LIBC

  return hypot(__x, __y);

#else

  // TODO: implement
  return 0.0;

#endif
#endif
}

int REDIRECT_NAME(ilogb)(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._ilogb(__x);

#else

  libc_trace("ilogb");debug_write("ilogb");

#ifdef USE_HOST_LIBC

  return ilogb(__x);

#else

  // TODO: implement
  return 0;

#endif
#endif
}

double REDIRECT_NAME(log)(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._log(__x);

#else

  libc_trace("log");

#ifdef USE_HOST_LIBC

  return log(__x);

#else

  {
    static double pospow2[10] =
      { 2.0, 4.0, 16.0, 256.0, 65536.0, 4294967296.0,
        1.8446744073709552e19, 3.402823669209385e38,
        1.157920892373162e77, 1.3407807929942597e154
      };
    static double negpow2[10] =
      { 0.5, 0.25, 0.0625, 0.00390625, 1.52587890625e-5,
        2.3283064365386963e-10, 5.421010862427522e-20,
        2.938735877055719e-39, 8.636168555094445e-78,
        7.458340731200207e-155
      };
    double y = __x;
    double a;
    double b;
    double r;
    int i = 9;
    int e = 0;
    int p;
    if (y < 1) {
      while (i >= 0) {
        if (y <= negpow2[i]) {
          y *= pospow2[i];
          e -= (1<<i);
        }
        i--;
      }
      e++;
      y *= 2;
    } else {
      while (i >= 0) {
        if (y >= pospow2[i]) {
          y *= negpow2[i];
          e += (1<<i);
        }
        i--;
      }
    }
    a = (y-1)/(y+1);
    b = a*a;
    r = a;
    p = 3;
#define STEP a *= b; r += a/p; p += 2;
    STEP; STEP; STEP; STEP; STEP; STEP; STEP;
    return e * 0.6931471805599453 + 2 * r;
  }

#endif
#endif
}

double REDIRECT_NAME(log1p)(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._log1p(__x);

#else

  libc_trace("log1p");debug_write("log1p");

#ifdef USE_HOST_LIBC

  return log1p(__x);

#else

  // TODO: implement
  return 0.0;

#endif
#endif
}

double REDIRECT_NAME(modf)(double __x, double *__iptr) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._modf(__x, __iptr);

#else

  libc_trace("modf");

#ifdef USE_HOST_LIBC

  return modf(__x, __iptr);

#else

  if (__x == 0.0) {
    return *__iptr = __x;
  } else if (__x < 0.0) {
    return __x - (*__iptr = ceil(__x));
  } else {
    return __x - (*__iptr = floor(__x));
  }

#endif
#endif
}

double REDIRECT_NAME(pow)(double __x, double __y) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._pow(__x, __y);

#else

  libc_trace("pow");debug_write("pow");

#ifdef USE_HOST_LIBC

  return pow(__x, __y);

#else

  // TODO: implement
  return 0.0;

#endif
#endif
}

double REDIRECT_NAME(sin)(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._sin(__x);

#else

  libc_trace("sin");debug_write("sin");

#ifdef USE_HOST_LIBC

  return sin(__x);

#else

  // TODO: implement
  return 0.0;

#endif
#endif
}

double REDIRECT_NAME(sinh)(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._sinh(__x);

#else

  libc_trace("sinh");debug_write("sinh");

#ifdef USE_HOST_LIBC

  return sinh(__x);

#else

  // TODO: implement
  return 0.0;

#endif
#endif
}

double REDIRECT_NAME(sqrt)(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._sqrt(__x);

#else

  libc_trace("sqrt");debug_write("sqrt");

#ifdef USE_HOST_LIBC

  return sqrt(__x);

#else

  return 0.0; // TODO: implement

#endif
#endif
}

double REDIRECT_NAME(tan)(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._tan(__x);

#else

  libc_trace("tan");debug_write("tan");

#ifdef USE_HOST_LIBC

  return tan(__x);

#else

  // TODO: implement
  return 0.0;

#endif
#endif
}

double REDIRECT_NAME(tanh)(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._tanh(__x);

#else

  libc_trace("tanh");debug_write("tanh");

#ifdef USE_HOST_LIBC

  return tanh(__x);

#else

  // TODO: implement
  return 0.0;

#endif
#endif
}

double REDIRECT_NAME(scalbn)(double __x, int __exp) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._scalbn(__x, __exp);

#else

  libc_trace("scalbn");debug_write("scalbn");

#ifdef USE_HOST_LIBC

  return scalbn(__x, __exp);

#else

  // TODO: implement
  return 0.0;

#endif
#endif
}

#ifndef USE_LIBC_LINK

void libc_init_math(void) {
}

#endif
