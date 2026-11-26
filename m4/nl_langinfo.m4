# nl_langinfo.m4
# serial 14
dnl Copyright (C) 2009-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

AC_DEFUN([gl_FUNC_NL_LANGINFO],
[
  AC_REQUIRE([gl_LANGINFO_H_DEFAULTS])
  AC_REQUIRE([gl_LANGINFO_H])
  gl_CHECK_FUNCS_ANDROID([nl_langinfo], [[#include <langinfo.h>]])
  AC_REQUIRE([AC_CANONICAL_HOST])
  AC_REQUIRE([gl_FUNC_SETLOCALE_NULL])
  AC_REQUIRE([gl_PTHREADLIB])
  AC_CHECK_HEADERS_ONCE([threads.h])
  if test $ac_cv_func_nl_langinfo = yes; then
    # On macOS 26, Solaris 10, and Solaris 11.3, nl_langinfo is not
    # thread-safe.
    case "$host_os" in
      darwin* | solaris*) NL_LANGINFO_MTSAFE=0 ;;
      *)                  NL_LANGINFO_MTSAFE=1 ;;
    esac
    AC_DEFINE_UNQUOTED([NL_LANGINFO_MTSAFE], [$NL_LANGINFO_MTSAFE],
      [Define to 1 if nl_langinfo is thread-safe.])
    if test $HAVE_LANGINFO_CODESET = 1 \
       && test $HAVE_LANGINFO_ALTMON = 1 \
       && test $HAVE_LANGINFO_ABALTMON = 1 \
       && test $HAVE_LANGINFO_ERA = 1 \
       && test $NL_LANGINFO_MTSAFE = 1; then
      :
    else
      REPLACE_NL_LANGINFO=1
      AC_DEFINE([REPLACE_NL_LANGINFO], [1],
        [Define if nl_langinfo exists but is overridden by gnulib.])
    fi
  else
    HAVE_NL_LANGINFO=0
    case "$gl_cv_onwards_func_nl_langinfo" in
      future*) REPLACE_NL_LANGINFO=1 ;;
    esac
  fi
  if test $HAVE_NL_LANGINFO = 0 || test $HAVE_LANGINFO_CODESET = 0; then
    LIB_NL_LANGINFO="$SETLOCALE_NULL_LIB"
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
