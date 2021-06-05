# localeconv.m4 serial 1
dnl Copyright (C) 2012-2021 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_LOCALECONV],
[
  AC_REQUIRE([gl_LOCALE_H_DEFAULTS])
  AC_REQUIRE([gl_LOCALE_H])

  if test $REPLACE_STRUCT_LCONV = 1; then
    REPLACE_LOCALECONV=1
  fi
])

# Prerequisites of lib/localeconv.c.
AC_DEFUN([gl_PREREQ_LOCALECONV],
[
  AC_CHECK_MEMBERS([struct lconv.decimal_point], [], [],
    [[#include <locale.h>]])
])
