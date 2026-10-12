# wctype.m4
# serial 6
dnl Copyright (C) 2011-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

AC_DEFUN_ONCE([gl_FUNC_WCTYPE],
[
  AC_REQUIRE([gl_WCTYPE_H_DEFAULTS])
  AC_REQUIRE([gl_WCTYPE_H])
  AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
  HAVE_WCTYPE=$HAVE_WCTYPE_T
  if test $HAVE_WCTYPE = 1; then
    AC_CACHE_CHECK([whether wctype supports the "blank" and "punct" character classes],
      [gl_cv_func_wctype_works],
      [AC_RUN_IFELSE(
         [AC_LANG_SOURCE([[
            #include <ctype.h>
            #include <wchar.h>
            #include <wctype.h>
            int main ()
            {
              /* This test fails on mingw.  */
              if (wctype ("blank") == (wctype_t)0)
                return 1;
              /* This test fails on MSVC 14.  */
              if ((! iswctype ('\t', wctype ("blank"))) != (! iswblank ('\t')))
                return 2;
              /* This test fails on Android 11.  */
              if ((! iswctype ('\`', wctype ("punct"))) != (! ispunct ('\`')))
                return 4;
              return 0;
            }
         ]])],
         [gl_cv_func_wctype_works=yes], [gl_cv_func_wctype_works=no],
         [case "$host_os" in
                               # Guess no on native Windows.
            mingw* | windows*) gl_cv_func_wctype_works="guessing no" ;;
                               # Guess no on Android.
            android*)          gl_cv_func_wctype_works="guessing no" ;;
                               # Guess yes otherwise.
            *)                 gl_cv_func_wctype_works="guessing yes" ;;
          esac
         ])
      ])
    case "$gl_cv_func_wctype_works" in
      *yes) ;;
      *) REPLACE_WCTYPE=1 ;;
    esac
  fi
])
