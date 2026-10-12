# iswpunct.m4
# serial 2
dnl Copyright (C) 2023-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

AC_DEFUN([gl_FUNC_ISWPUNCT],
[
  AC_REQUIRE([gl_WCTYPE_H_DEFAULTS])
  AC_REQUIRE([gl_WCTYPE_H])
  AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles

  if test $HAVE_ISWCNTRL = 0 || test $REPLACE_ISWCNTRL = 1; then
    dnl <wctype.h> redefines iswpunct already.
    REPLACE_ISWPUNCT="$REPLACE_ISWCNTRL"
  else
    AC_CACHE_CHECK([whether iswpunct is consistent with ispunct],
      [gl_cv_func_iswpunct_works],
      [AC_RUN_IFELSE(
         [AC_LANG_SOURCE([[
#include <ctype.h>
#include <wchar.h>
#include <wctype.h>
int
main (int argc, char *argv[])
{
  int result = 0;
  /* This fails on Android 11.  */
  if ((! iswpunct ('\`')) != (! ispunct ('\`')))
    result |= 1;
  return result;
}]])],
         [gl_cv_func_iswpunct_works=yes],
         [gl_cv_func_iswpunct_works=no],
         [case "$host_os" in
            # Guess no on Android.
            android*) gl_cv_func_iswpunct_works="guessing no" ;;
            # Guess yes otherwise.
            *)        gl_cv_func_iswpunct_works="guessing yes" ;;
          esac
         ])
      ])
    case "$gl_cv_func_iswpunct_works" in
      *yes) ;;
      *) REPLACE_ISWPUNCT=1 ;;
    esac
  fi
])
