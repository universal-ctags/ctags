# nl_langinfo.m4 serial 8
dnl Copyright (C) 2009-2021 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_NL_LANGINFO],
[
  AC_REQUIRE([gl_LANGINFO_H_DEFAULTS])
  AC_REQUIRE([gl_LANGINFO_H])
  AC_CHECK_FUNCS_ONCE([nl_langinfo])
  AC_REQUIRE([AC_CANONICAL_HOST])
  AC_REQUIRE([gl_FUNC_SETLOCALE_NULL])
  AC_REQUIRE([gl_PTHREADLIB])
  AC_CHECK_HEADERS_ONCE([threads.h])
  if test $ac_cv_func_nl_langinfo = yes; then
    # On Irix 6.5, YESEXPR is defined, but nl_langinfo(YESEXPR) is broken.
    AC_CACHE_CHECK([whether YESEXPR works],
      [gl_cv_func_nl_langinfo_yesexpr_works],
      [AC_RUN_IFELSE(
         [AC_LANG_PROGRAM([[#include <langinfo.h>
]], [[return !*nl_langinfo(YESEXPR);
]])],
         [gl_cv_func_nl_langinfo_yesexpr_works=yes],
         [gl_cv_func_nl_langinfo_yesexpr_works=no],
         [
         case "$host_os" in
                   # Guess no on irix systems.
           irix*)  gl_cv_func_nl_langinfo_yesexpr_works="guessing no";;
                   # Guess yes elsewhere.
           *)      gl_cv_func_nl_langinfo_yesexpr_works="guessing yes";;
         esac
         ])
      ])
    case $gl_cv_func_nl_langinfo_yesexpr_works in
      *yes) FUNC_NL_LANGINFO_YESEXPR_WORKS=1 ;;
      *)    FUNC_NL_LANGINFO_YESEXPR_WORKS=0 ;;
    esac
    AC_DEFINE_UNQUOTED([FUNC_NL_LANGINFO_YESEXPR_WORKS],
      [$FUNC_NL_LANGINFO_YESEXPR_WORKS],
      [Define to 1 if nl_langinfo (YESEXPR) returns a non-empty string.])
    # On Solaris 10 and Solaris 11.3, nl_langinfo is not multithread-safe.
    case "$host_os" in
      solaris*) NL_LANGINFO_MTSAFE=0 ;;
      *)        NL_LANGINFO_MTSAFE=1 ;;
    esac
    AC_DEFINE_UNQUOTED([NL_LANGINFO_MTSAFE], [$NL_LANGINFO_MTSAFE],
      [Define to 1 if nl_langinfo is multithread-safe.])
    if test $HAVE_LANGINFO_CODESET = 1 \
       && test $HAVE_LANGINFO_T_FMT_AMPM = 1 \
       && test $HAVE_LANGINFO_ALTMON = 1 \
       && test $HAVE_LANGINFO_ERA = 1 \
       && test $FUNC_NL_LANGINFO_YESEXPR_WORKS = 1 \
       && test $NL_LANGINFO_MTSAFE = 1; then
      :
    else
      REPLACE_NL_LANGINFO=1
      AC_DEFINE([REPLACE_NL_LANGINFO], [1],
        [Define if nl_langinfo exists but is overridden by gnulib.])
    fi
  else
    HAVE_NL_LANGINFO=0
  fi
  if test $HAVE_NL_LANGINFO = 0 || test $HAVE_LANGINFO_CODESET = 0; then
    LIB_NL_LANGINFO="$LIB_SETLOCALE_NULL"
  else
    LIB_NL_LANGINFO=
  fi
  dnl LIB_NL_LANGINFO is expected to be empty everywhere.
  AC_SUBST([LIB_NL_LANGINFO])
])

# Prerequisites of lib/nl_langinfo-lock.c.
AC_DEFUN([gl_PREREQ_NL_LANGINFO_LOCK],
[
  gl_VISIBILITY
])
