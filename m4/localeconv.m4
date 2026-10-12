# localeconv.m4
# serial 4
dnl Copyright (C) 2012-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

AC_DEFUN([gl_FUNC_LOCALECONV],
[
  AC_REQUIRE([gl_LOCALE_H_DEFAULTS])
  AC_REQUIRE([gl_LOCALE_H])
  AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles

  if test $REPLACE_STRUCT_LCONV = 1; then
    REPLACE_LOCALECONV=1
  fi
  if test $REPLACE_LOCALECONV = 0; then
    dnl Test whether fields of type 'char' are filled correctly.
    dnl This test fails on mingw 5.0.3.
    AC_CACHE_CHECK([whether localeconv works],
      [gl_cv_func_localeconv_works],
      [AC_RUN_IFELSE(
         [AC_LANG_SOURCE([[
            #include <locale.h>
            #include <limits.h>
            int main ()
            {
              const struct lconv *l = localeconv ();
              return l->frac_digits != CHAR_MAX && l->frac_digits < 0;
            }
         ]])],
         [gl_cv_func_localeconv_works=yes],
         [gl_cv_func_localeconv_works=no],
         [case "$host_os" in
                                # Guess yes on glibc systems.
            *-gnu* | gnu*)      gl_cv_func_localeconv_works="guessing yes" ;;
                                # Guess yes on musl systems.
            *-musl* | midipix*) gl_cv_func_localeconv_works="guessing yes" ;;
                                # Guess no on native Windows.
            mingw* | windows*)  gl_cv_func_localeconv_works="guessing no" ;;
                                # If we don't know, obey --enable-cross-guesses.
            *)                  gl_cv_func_localeconv_works="$gl_cross_guess_normal" ;;
          esac
         ])
      ])
    case "$gl_cv_func_localeconv_works" in
      *yes) ;;
      *) REPLACE_LOCALECONV=1 ;;
    esac
  fi
])

# Prerequisites of lib/localeconv.c.
AC_DEFUN([gl_PREREQ_LOCALECONV],
[
  AC_CHECK_MEMBERS([struct lconv.decimal_point], [], [],
    [[#include <locale.h>]])
  AC_CHECK_MEMBERS([struct lconv.int_p_cs_precedes], [], [],
    [[#include <locale.h>]])
])
