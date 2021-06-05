# mbsinit.m4 serial 9
dnl Copyright (C) 2008, 2010-2021 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_MBSINIT],
[
  AC_REQUIRE([gl_WCHAR_H_DEFAULTS])
  AC_REQUIRE([AC_CANONICAL_HOST])

  AC_REQUIRE([AC_TYPE_MBSTATE_T])
  gl_MBSTATE_T_BROKEN

  AC_CHECK_FUNCS_ONCE([mbsinit])
  if test $ac_cv_func_mbsinit = no; then
    HAVE_MBSINIT=0
    AC_CHECK_DECLS([mbsinit],,, [[
      #include <wchar.h>
    ]])
    if test $ac_cv_have_decl_mbsinit = yes; then
      dnl On Minix 3.1.8, the system's <wchar.h> declares mbsinit() although
      dnl it does not have the function. Avoid a collision with gnulib's
      dnl replacement.
      REPLACE_MBSINIT=1
    fi
  else
    if test $REPLACE_MBSTATE_T = 1; then
      REPLACE_MBSINIT=1
    else
      dnl On mingw, mbsinit() always returns 1, which is inappropriate for
      dnl states produced by mbrtowc() for an incomplete multibyte character
      dnl in multibyte locales.
      case "$host_os" in
        mingw*) REPLACE_MBSINIT=1 ;;
      esac
    fi
  fi
])

# Prerequisites of lib/mbsinit.c.
AC_DEFUN([gl_PREREQ_MBSINIT], [
  :
])
