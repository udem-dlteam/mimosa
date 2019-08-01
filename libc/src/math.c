#include "include/libc_common.h"
#include "include/math.h"

#ifndef USE_LIBC_LINK
#ifndef USE_HOST_LIBC
// to prevent optimizations by C compiler...
volatile double TWO_POW_52_1 = 4.503599627370496e15;
volatile double TWO_POW_52_2 = 4.503599627370496e15;
#endif
#endif

double acos(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._acos(__x);

#else

  libc_trace("acos");debug_write("acos");

#ifdef USE_HOST_LIBC

#undef acos

  return acos(__x);

#else

  // TODO: implement
  return 0.0;

#endif
#endif
}

double acosh(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._acosh(__x);

#else

  libc_trace("acosh");debug_write("acosh");

#ifdef USE_HOST_LIBC

#undef acosh

  return acosh(__x);

#else

  // TODO: implement
  return 0.0;

#endif
#endif
}

double asin(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._asin(__x);

#else

  libc_trace("asin");debug_write("asin");

#ifdef USE_HOST_LIBC

#undef asin

  return asin(__x);

#else

  // TODO: implement
  return 0.0;

#endif
#endif
}

double asinh(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._asinh(__x);

#else

  libc_trace("asinh");debug_write("asinh");

#ifdef USE_HOST_LIBC

#undef asinh

  return asinh(__x);

#else

  // TODO: implement
  return 0.0;

#endif
#endif
}

double atan(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._atan(__x);

#else

  libc_trace("atan");debug_write("atan");

#ifdef USE_HOST_LIBC

#undef atan

  return atan(__x);

#else

  // TODO: implement
  return 0.0;

#endif
#endif
}

double atan2(double __y, double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._atan2(__y, __x);

#else

  libc_trace("atan2");debug_write("atan2");

#ifdef USE_HOST_LIBC

#undef atan2

  return atan2(__y, __x);

#else

  // TODO: implement
  return 0.0;

#endif
#endif
}

double atanh(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._atanh(__x);

#else

  libc_trace("atanh");debug_write("atanh");

#ifdef USE_HOST_LIBC

#undef atanh

  return atanh(__x);

#else

  // TODO: implement
  return 0.0;

#endif
#endif
}

double ceil(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._ceil(__x);

#else

  libc_trace("ceil");

#ifdef USE_HOST_LIBC

#undef ceil

  return ceil(__x);

#else

  {
    double a = (__x < 0) ? -__x : __x;
    if (a >= TWO_POW_52_1) {
      debug_write("ceil");
      debug_write(__x*1000);
      debug_write(__x*1000);
      return __x; // no possible fractional part
    } else {
      double b = (a + TWO_POW_52_1) - TWO_POW_52_2;
      if (b > a) b = b-1;
      if (__x < 0)
        b = -b;
      if (b != __x)
        b = b+1;
      debug_write("ceil");
      debug_write(__x*1000);
      debug_write(b*1000);
      return b;
    }
  }

#endif
#endif
}

double cos(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._cos(__x);

#else

  libc_trace("cos");debug_write("cos");

#ifdef USE_HOST_LIBC

#undef cos

  return cos(__x);

#else

  return 0.0; // TODO: implement

#endif
#endif
}

double cosh(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._cosh(__x);

#else

  libc_trace("cosh");debug_write("cosh");

#ifdef USE_HOST_LIBC

#undef cosh

  return cosh(__x);

#else

  // TODO: implement
  return 0.0;

#endif
#endif
}

double exp(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._exp(__x);

#else

  libc_trace("exp");debug_write("exp");

#ifdef USE_HOST_LIBC

#undef exp

  return exp(__x);

#else

  // TODO: implement
  return 0.0;

#endif
#endif
}

double expm1(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._expm1(__x);

#else

  libc_trace("expm1");debug_write("expm1");

#ifdef USE_HOST_LIBC

#undef expm1

  return expm1(__x);

#else

  // TODO: implement
  return 0.0;

#endif
#endif
}

double fabs(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._fabs(__x);

#else

  libc_trace("fabs");

#ifdef USE_HOST_LIBC

#undef fabs

  return fabs(__x);

#else

  if (__x < 0 || (__x == 0 && 1/__x < 0)) // correctly handle -0.0
    return -__x;
  else
    return __x;

#endif
#endif
}

double floor(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._floor(__x);

#else

  libc_trace("floor");

#ifdef USE_HOST_LIBC

#undef floor

  return floor(__x);

#else

  {
    double a = (__x < 0) ? -__x : __x;
    if (a >= TWO_POW_52_1) {
      debug_write("floor");
      debug_write(__x*1000);
      debug_write(__x*1000);
      return __x; // no possible fractional part
    } else {
      double b = (a + TWO_POW_52_1) - TWO_POW_52_2;
      if (b > a) b = b-1;
      if (__x < 0)
        b = -b;
      debug_write("floor");
      debug_write(__x*1000);
      debug_write(b*1000);
      return b;
    }
  }

#endif
#endif
}

double hypot(double __x, double __y) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._hypot(__x, __y);

#else

  libc_trace("hypot");debug_write("hypot");

#ifdef USE_HOST_LIBC

#undef hypot

  return hypot(__x, __y);

#else

  // TODO: implement
  return 0.0;

#endif
#endif
}

int ilogb(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._ilogb(__x);

#else

  libc_trace("ilogb");debug_write("ilogb");

#ifdef USE_HOST_LIBC

#undef ilogb

  return ilogb(__x);

#else

  // TODO: implement
  return 0;

#endif
#endif
}

double log(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._log(__x);

#else

  libc_trace("log");

#ifdef USE_HOST_LIBC

#undef log

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

double log1p(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._log1p(__x);

#else

  libc_trace("log1p");debug_write("log1p");

#ifdef USE_HOST_LIBC

#undef log1p

  return log1p(__x);

#else

  // TODO: implement
  return 0.0;

#endif
#endif
}

double modf(double __x, double *__iptr) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._modf(__x, __iptr);

#else

  libc_trace("modf");

#ifdef USE_HOST_LIBC

#undef modf

  return modf(__x, __iptr);

#else

  {
    double a = (__x < 0) ? -__x : __x;
    if (a >= TWO_POW_52_1) {
      *__iptr = __x; // no possible fractional part
      return 0.0;
    } else {
      double b = (a + TWO_POW_52_1) - TWO_POW_52_2;
      if (b > a) b = b-1;
      if (__x < 0)
        b = -b;
      *__iptr = b; // no possible fractional part
      return __x - b;
    }
  }

#endif
#endif
}

double pow(double __x, double __y) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._pow(__x, __y);

#else

  libc_trace("pow");debug_write("pow");

#ifdef USE_HOST_LIBC

#undef pow

  return pow(__x, __y);

#else

  // TODO: implement
  return 0.0;

#endif
#endif
}

double sin(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._sin(__x);

#else

  libc_trace("sin");debug_write("sin");

#ifdef USE_HOST_LIBC

#undef sin

  return sin(__x);

#else

  return 0.0; // TODO: implement

#endif
#endif
}

double sinh(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._sinh(__x);

#else

  libc_trace("sinh");debug_write("sinh");

#ifdef USE_HOST_LIBC

#undef sinh

  return sinh(__x);

#else

  // TODO: implement
  return 0.0;

#endif
#endif
}

double sqrt(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._sqrt(__x);

#else

  libc_trace("sqrt");debug_write("sqrt");

#ifdef USE_HOST_LIBC

#undef sqrt

  return sqrt(__x);

#else

  return 0.0; // TODO: implement

#endif
#endif
}

double tan(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._tan(__x);

#else

  libc_trace("tan");debug_write("tan");

#ifdef USE_HOST_LIBC

#undef tan

  return tan(__x);

#else

  return 0.0; // TODO: implement

#endif
#endif
}

double tanh(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._tanh(__x);

#else

  libc_trace("tanh");debug_write("tanh");

#ifdef USE_HOST_LIBC

#undef tanh

  return tanh(__x);

#else

  // TODO: implement
  return 0.0;

#endif
#endif
}

double scalbn(double __x, int __exp) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._scalbn(__x, __exp);

#else

  libc_trace("scalbn");debug_write("scalbn");

#ifdef USE_HOST_LIBC

#undef scalbn

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
