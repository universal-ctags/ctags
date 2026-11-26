# limits-h.m4
# serial 1
dnl Copyright 2016-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

dnl Check whether limits.h has needed features.

dnl From Paul Eggert.

AC_DEFUN_ONCE([gl_LIMITS_H],
[
  gl_CHECK_NEXT_HEADERS([limits.h])

  AC_CACHE_CHECK([whether limits.h has WORD_BIT, BOOL_WIDTH etc.],
    [gl_cv_header_limits_width],
    [AC_COMPILE_IFELSE(
       [AC_LANG_PROGRAM(
          [[#ifndef __STDC_WANT_IEC_60559_BFP_EXT__
             #define __STDC_WANT_IEC_60559_BFP_EXT__ 1
            #endif
            #include <limits.h>
            long long llm = LLONG_MAX;
            int wb = WORD_BIT;
            int ullw = ULLONG_WIDTH;
            int bw = BOOL_WIDTH;
            int bm = BOOL_MAX;
            int mblm = MB_LEN_MAX;
          ]])],
       [gl_cv_header_limits_width=yes],
       [gl_cv_header_limits_width=no])])
  GL_GENERATE_LIMITS_H=true
  AS_IF([test "$gl_cv_header_limits_width" = yes],
    [AC_CACHE_CHECK([whether limits.h has SSIZE_MAX],
       [gl_cv_header_limits_ssize_max],
       [AC_COMPILE_IFELSE(
          [AC_LANG_SOURCE(
             [[#include <limits.h>
               #ifndef SSIZE_MAX
                 #error "SSIZE_MAX is not defined"
               #endif
             ]])],
          [gl_cv_header_limits_ssize_max=yes],
          [gl_cv_header_limits_ssize_max=no])])
     if test "$gl_cv_header_limits_ssize_max" = yes; then
       GL_GENERATE_LIMITS_H=false
     fi])
])

dnl Unconditionally enables the replacement of <limits.h>.
AC_DEFUN([gl_REPLACE_LIMITS_H],
[
  AC_REQUIRE([gl_LIMITS_H])
  GL_GENERATE_LIMITS_H=true
])
