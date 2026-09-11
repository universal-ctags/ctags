# wmempcpy.m4
# serial 3
dnl Copyright (C) 2020-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

AC_DEFUN([gl_FUNC_WMEMPCPY],
[
  AC_REQUIRE([gl_WCHAR_H_DEFAULTS])

  dnl Persuade glibc <wchar.h> to declare wmempcpy().
  AC_REQUIRE([AC_USE_SYSTEM_EXTENSIONS])

  dnl The wmempcpy() declaration in lib/wchar.in.h uses 'restrict'.
  AC_REQUIRE([AC_C_RESTRICT])

  gl_CHECK_FUNCS_ANDROID([wmempcpy], [[#include <wchar.h>]])
  if test $ac_cv_func_wmempcpy = no; then
    HAVE_WMEMPCPY=0
    case "$gl_cv_onwards_func_wmempcpy" in
      future*) REPLACE_WMEMPCPY=1 ;;
    esac
  fi
])
