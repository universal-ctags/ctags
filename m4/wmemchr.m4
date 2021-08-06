# wmemchr.m4 serial 5
dnl Copyright (C) 2011-2021 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_WMEMCHR],
[
  AC_REQUIRE([gl_WCHAR_H_DEFAULTS])
  dnl We cannot use AC_CHECK_FUNCS here, because the MSVC 9 header files
  dnl provide this function as an inline function definition.
  AC_CACHE_CHECK([for wmemchr], [gl_cv_func_wmemchr],
    [AC_LINK_IFELSE(
       [AC_LANG_PROGRAM(
          [[#include <wchar.h>
          ]],
          [[return ! wmemchr ((const wchar_t *) 0, (wchar_t) ' ', 0);]])
       ],
       [gl_cv_func_wmemchr=yes],
       [gl_cv_func_wmemchr=no])
    ])
  if test $gl_cv_func_wmemchr = no; then
    HAVE_WMEMCHR=0
  fi
])
