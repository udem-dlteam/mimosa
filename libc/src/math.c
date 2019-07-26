#include "include/libc_common.h"
#include "include/math.h"

double acos(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._acos(__x);

#else

  libc_trace("acos");

#ifdef USE_HOST_LIBC

#undef acos

  return acos(__x);

#else

  /* TODO: implement */
  return 0.0;

#endif
#endif
}

double acosh(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._acosh(__x);

#else

  libc_trace("acosh");

#ifdef USE_HOST_LIBC

#undef acosh

  return acosh(__x);

#else

  /* TODO: implement */
  return 0.0;

#endif
#endif
}

double asin(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._asin(__x);

#else

  libc_trace("asin");

#ifdef USE_HOST_LIBC

#undef asin

  return asin(__x);

#else

  /* TODO: implement */
  return 0.0;

#endif
#endif
}

double asinh(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._asinh(__x);

#else

  libc_trace("asinh");

#ifdef USE_HOST_LIBC

#undef asinh

  return asinh(__x);

#else

  /* TODO: implement */
  return 0.0;

#endif
#endif
}

double atan(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._atan(__x);

#else

  libc_trace("atan");

#ifdef USE_HOST_LIBC

#undef atan

  return atan(__x);

#else

  /* TODO: implement */
  return 0.0;

#endif
#endif
}

double atan2(double __y, double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._atan2(__y, __x);

#else

  libc_trace("atan2");

#ifdef USE_HOST_LIBC

#undef atan2

  return atan2(__y, __x);

#else

  /* TODO: implement */
  return 0.0;

#endif
#endif
}

double atanh(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._atanh(__x);

#else

  libc_trace("atanh");

#ifdef USE_HOST_LIBC

#undef atanh

  return atanh(__x);

#else

  /* TODO: implement */
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

  /* TODO: implement */
  return 0.0;

#endif
#endif
}

double cos(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._cos(__x);

#else

  libc_trace("cos");

#ifdef USE_HOST_LIBC

#undef cos

  return cos(__x);

#else

  /* TODO: implement */
  return 0.0;

#endif
#endif
}

double cosh(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._cosh(__x);

#else

  libc_trace("cosh");

#ifdef USE_HOST_LIBC

#undef cosh

  return cosh(__x);

#else

  /* TODO: implement */
  return 0.0;

#endif
#endif
}

double exp(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._exp(__x);

#else

  libc_trace("exp");

#ifdef USE_HOST_LIBC

#undef exp

  return exp(__x);

#else

  /* TODO: implement */
  return 0.0;

#endif
#endif
}

double expm1(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._expm1(__x);

#else

  libc_trace("expm1");

#ifdef USE_HOST_LIBC

#undef expm1

  return expm1(__x);

#else

  /* TODO: implement */
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

  /* TODO: implement */
  return 0.0;

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

  /* TODO: implement */
  return 0.0;

#endif
#endif
}

double hypot(double __x, double __y) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._hypot(__x, __y);

#else

  libc_trace("hypot");

#ifdef USE_HOST_LIBC

#undef hypot

  return hypot(__x, __y);

#else

  /* TODO: implement */
  return 0.0;

#endif
#endif
}

int ilogb(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._ilogb(__x);

#else

  libc_trace("ilogb");

#ifdef USE_HOST_LIBC

#undef ilogb

  return ilogb(__x);

#else

  /* TODO: implement */
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

  /* TODO: implement */
  return 0.0;

#endif
#endif
}

double log1p(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._log1p(__x);

#else

  libc_trace("log1p");

#ifdef USE_HOST_LIBC

#undef log1p

  return log1p(__x);

#else

  /* TODO: implement */
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

  /* TODO: implement */
  return 0.0;

#endif
#endif
}

double pow(double __x, double __y) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._pow(__x, __y);

#else

  libc_trace("pow");

#ifdef USE_HOST_LIBC

#undef pow

  return pow(__x, __y);

#else

  /* TODO: implement */
  return 0.0;

#endif
#endif
}

double sin(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._sin(__x);

#else

  libc_trace("sin");

#ifdef USE_HOST_LIBC

#undef sin

  return sin(__x);

#else

  /* TODO: implement */
  return 0.0;

#endif
#endif
}

double sinh(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._sinh(__x);

#else

  libc_trace("sinh");

#ifdef USE_HOST_LIBC

#undef sinh

  return sinh(__x);

#else

  /* TODO: implement */
  return 0.0;

#endif
#endif
}

double sqrt(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._sqrt(__x);

#else

  libc_trace("sqrt");

#ifdef USE_HOST_LIBC

#undef sqrt

  return sqrt(__x);

#else

  /* TODO: implement */
  return 0.0;

#endif
#endif
}

double tan(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._tan(__x);

#else

  libc_trace("tan");

#ifdef USE_HOST_LIBC

#undef tan

  return tan(__x);

#else

  /* TODO: implement */
  return 0.0;

#endif
#endif
}

double tanh(double __x) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._tanh(__x);

#else

  libc_trace("tanh");

#ifdef USE_HOST_LIBC

#undef tanh

  return tanh(__x);

#else

  /* TODO: implement */
  return 0.0;

#endif
#endif
}

double scalbn(double __x, int __exp) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._scalbn(__x, __exp);

#else

  libc_trace("scalbn");

#ifdef USE_HOST_LIBC

#undef scalbn

  return scalbn(__x, __exp);

#else

  /* TODO: implement */
  return 0.0;

#endif
#endif
}

#ifndef USE_LIBC_LINK

void libc_init_math(void) {
}

#endif
