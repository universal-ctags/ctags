# mbtowc.m4
# serial 5
dnl Copyright (C) 2011-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

AC_DEFUN([gl_FUNC_MBTOWC],
[
  AC_REQUIRE([gl_STDLIB_H_DEFAULTS])

  gl_CHECK_FUNCS_ANDROID([mbtowc], [[#include <stdlib.h>]])
  if test $ac_cv_func_mbtowc = no; then
    HAVE_MBTOWC=0
    case "$gl_cv_onwards_func_mbtowc" in
      future*) REPLACE_MBTOWC=1 ;;
    esac
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
