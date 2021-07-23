# isblank.m4 serial 3
dnl Copyright (C) 2009-2021 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_ISBLANK],
[
  dnl Persuade glibc <ctype.h> to declare isblank().
  AC_REQUIRE([AC_USE_SYSTEM_EXTENSIONS])

  AC_REQUIRE([gl_CTYPE_H_DEFAULTS])
  AC_CHECK_FUNCS_ONCE([isblank])
  if test $ac_cv_func_isblank = no; then
    HAVE_ISBLANK=0
  fi
])
