# mbtowc.m4 serial 3
dnl Copyright (C) 2011-2021 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_MBTOWC],
[
  AC_REQUIRE([gl_STDLIB_H_DEFAULTS])

  AC_CHECK_FUNCS([mbtowc])
  if test $ac_cv_func_mbtowc = no; then
    HAVE_MBTOWC=0
  else
    if false; then
      REPLACE_MBTOWC=1
    fi
  fi
])

# Prerequisites of lib/mbtowc.c.
AC_DEFUN([gl_PREREQ_MBTOWC], [
  :
])
