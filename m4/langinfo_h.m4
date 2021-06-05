# langinfo_h.m4 serial 12
dnl Copyright (C) 2009-2021 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN_ONCE([gl_LANGINFO_H],
[
  AC_REQUIRE([gl_LANGINFO_H_DEFAULTS])

  dnl Persuade glibc-2.0.6 <langinfo.h> to define CODESET.
  AC_REQUIRE([AC_USE_SYSTEM_EXTENSIONS])

  dnl <langinfo.h> is always overridden, because of GNULIB_POSIXCHECK.
  gl_CHECK_NEXT_HEADERS([langinfo.h])

  dnl Determine whether <langinfo.h> exists. It is missing on mingw and BeOS.
  HAVE_LANGINFO_CODESET=0
  HAVE_LANGINFO_T_FMT_AMPM=0
  HAVE_LANGINFO_ALTMON=0
  HAVE_LANGINFO_ERA=0
  HAVE_LANGINFO_YESEXPR=0
  AC_CHECK_HEADERS_ONCE([langinfo.h])
  if test $ac_cv_header_langinfo_h = yes; then
    HAVE_LANGINFO_H=1
    dnl Determine what <langinfo.h> defines.
    dnl CODESET is missing on OpenBSD 3.8.
    dnl ERA etc. are missing on OpenBSD 6.7.
    dnl T_FMT_AMPM and YESEXPR, NOEXPR are missing on IRIX 5.3.
    dnl ALTMON_* are missing on glibc 2.26 and many other systems.
    AC_CACHE_CHECK([whether langinfo.h defines CODESET],
      [gl_cv_header_langinfo_codeset],
      [AC_COMPILE_IFELSE(
         [AC_LANG_PROGRAM([[#include <langinfo.h>
int a = CODESET;
]])],
         [gl_cv_header_langinfo_codeset=yes],
         [gl_cv_header_langinfo_codeset=no])
      ])
    if test $gl_cv_header_langinfo_codeset = yes; then
      HAVE_LANGINFO_CODESET=1
    fi
    AC_CACHE_CHECK([whether langinfo.h defines T_FMT_AMPM],
      [gl_cv_header_langinfo_t_fmt_ampm],
      [AC_COMPILE_IFELSE(
         [AC_LANG_PROGRAM([[#include <langinfo.h>
int a = T_FMT_AMPM;
]])],
         [gl_cv_header_langinfo_t_fmt_ampm=yes],
         [gl_cv_header_langinfo_t_fmt_ampm=no])
      ])
    if test $gl_cv_header_langinfo_t_fmt_ampm = yes; then
      HAVE_LANGINFO_T_FMT_AMPM=1
    fi
    AC_CACHE_CHECK([whether langinfo.h defines ALTMON_1],
      [gl_cv_header_langinfo_altmon],
      [AC_COMPILE_IFELSE(
         [AC_LANG_PROGRAM([[#include <langinfo.h>
int a = ALTMON_1;
]])],
         [gl_cv_header_langinfo_altmon=yes],
         [gl_cv_header_langinfo_altmon=no])
      ])
    if test $gl_cv_header_langinfo_altmon = yes; then
      HAVE_LANGINFO_ALTMON=1
    fi
    AC_CACHE_CHECK([whether langinfo.h defines ERA],
      [gl_cv_header_langinfo_era],
      [AC_COMPILE_IFELSE(
         [AC_LANG_PROGRAM([[#include <langinfo.h>
int a = ERA;
]])],
         [gl_cv_header_langinfo_era=yes],
         [gl_cv_header_langinfo_era=no])
      ])
    if test $gl_cv_header_langinfo_era = yes; then
      HAVE_LANGINFO_ERA=1
    fi
    AC_CACHE_CHECK([whether langinfo.h defines YESEXPR],
      [gl_cv_header_langinfo_yesexpr],
      [AC_COMPILE_IFELSE(
         [AC_LANG_PROGRAM([[#include <langinfo.h>
int a = YESEXPR;
]])],
         [gl_cv_header_langinfo_yesexpr=yes],
         [gl_cv_header_langinfo_yesexpr=no])
      ])
    if test $gl_cv_header_langinfo_yesexpr = yes; then
      HAVE_LANGINFO_YESEXPR=1
    fi
  else
    HAVE_LANGINFO_H=0
  fi
  AC_SUBST([HAVE_LANGINFO_H])
  AC_SUBST([HAVE_LANGINFO_CODESET])
  AC_SUBST([HAVE_LANGINFO_T_FMT_AMPM])
  AC_SUBST([HAVE_LANGINFO_ALTMON])
  AC_SUBST([HAVE_LANGINFO_ERA])
  AC_SUBST([HAVE_LANGINFO_YESEXPR])

  dnl Check for declarations of anything we want to poison if the
  dnl corresponding gnulib module is not in use.
  gl_WARN_ON_USE_PREPARE([[#include <langinfo.h>
    ]], [nl_langinfo])
])

# gl_LANGINFO_MODULE_INDICATOR([modulename])
# sets the shell variable that indicates the presence of the given module
# to a C preprocessor expression that will evaluate to 1.
# This macro invocation must not occur in macros that are AC_REQUIREd.
AC_DEFUN([gl_LANGINFO_MODULE_INDICATOR],
[
  dnl Ensure to expand the default settings once only.
  gl_LANGINFO_H_REQUIRE_DEFAULTS
  gl_MODULE_INDICATOR_SET_VARIABLE([$1])
  dnl Define it also as a C macro, for the benefit of the unit tests.
  gl_MODULE_INDICATOR_FOR_TESTS([$1])
])

# Initializes the default values for AC_SUBSTed shell variables.
# This macro must not be AC_REQUIREd.  It must only be invoked, and only
# outside of macros or in macros that are not AC_REQUIREd.
AC_DEFUN([gl_LANGINFO_H_REQUIRE_DEFAULTS],
[
  m4_defun(GL_MODULE_INDICATOR_PREFIX[_LANGINFO_H_MODULE_INDICATOR_DEFAULTS], [
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_NL_LANGINFO])
  ])
  m4_require(GL_MODULE_INDICATOR_PREFIX[_LANGINFO_H_MODULE_INDICATOR_DEFAULTS])
  AC_REQUIRE([gl_LANGINFO_H_DEFAULTS])
])

AC_DEFUN([gl_LANGINFO_H_DEFAULTS],
[
  dnl Assume proper GNU behavior unless another module says otherwise.
  HAVE_NL_LANGINFO=1;    AC_SUBST([HAVE_NL_LANGINFO])
  REPLACE_NL_LANGINFO=0; AC_SUBST([REPLACE_NL_LANGINFO])
])
