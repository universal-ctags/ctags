# wcrtomb.m4 serial 17
dnl Copyright (C) 2008-2021 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_WCRTOMB],
[
  AC_REQUIRE([gl_WCHAR_H_DEFAULTS])

  AC_REQUIRE([AC_TYPE_MBSTATE_T])
  gl_MBSTATE_T_BROKEN

  AC_CHECK_FUNCS_ONCE([wcrtomb])
  if test $ac_cv_func_wcrtomb = no; then
    HAVE_WCRTOMB=0
    AC_CHECK_DECLS([wcrtomb],,, [[
      #include <wchar.h>
    ]])
    if test $ac_cv_have_decl_wcrtomb = yes; then
      dnl On Minix 3.1.8, the system's <wchar.h> declares wcrtomb() although
      dnl it does not have the function. Avoid a collision with gnulib's
      dnl replacement.
      REPLACE_WCRTOMB=1
    fi
  else
    dnl We don't actually need to override wcrtomb when redefining the semantics
    dnl of the mbstate_t type. Tested on 32-bit AIX.
    dnl if test $REPLACE_MBSTATE_T = 1; then
    dnl   REPLACE_WCRTOMB=1
    dnl fi
    if test $REPLACE_WCRTOMB = 0; then
      dnl On Android 4.3, wcrtomb produces wrong characters in the C locale.
      dnl On AIX 4.3, OSF/1 5.1 and Solaris <= 11.3, wcrtomb (NULL, 0, NULL)
      dnl sometimes returns 0 instead of 1.
      AC_REQUIRE([AC_PROG_CC])
      AC_REQUIRE([gt_LOCALE_FR])
      AC_REQUIRE([gt_LOCALE_FR_UTF8])
      AC_REQUIRE([gt_LOCALE_JA])
      AC_REQUIRE([gt_LOCALE_ZH_CN])
      AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
      AC_CACHE_CHECK([whether wcrtomb works in the C locale],
        [gl_cv_func_wcrtomb_works],
        [AC_RUN_IFELSE(
           [AC_LANG_SOURCE([[
#include <string.h>
#include <stdlib.h>
#include <wchar.h>
int main ()
{
  mbstate_t state;
  char out[64];
  int count;
  memset (&state, 0, sizeof (state));
  out[0] = 'x';
  count = wcrtomb (out, L'a', &state);
  return !(count == 1 && out[0] == 'a');
}]])],
           [gl_cv_func_wcrtomb_works=yes],
           [gl_cv_func_wcrtomb_works=no],
           [case "$host_os" in
                               # Guess no on Android.
              linux*-android*) gl_cv_func_wcrtomb_works="guessing no";;
                               # Guess yes otherwise.
              *)               gl_cv_func_wcrtomb_works="guessing yes";;
            esac
           ])
        ])
      case "$gl_cv_func_wcrtomb_works" in
        *yes) ;;
        *) AC_DEFINE([WCRTOMB_C_LOCALE_BUG], [1],
             [Define if the wcrtomb function does not work in the C locale.])
           REPLACE_WCRTOMB=1 ;;
      esac
    fi
    if test $REPLACE_WCRTOMB = 0; then
      AC_CACHE_CHECK([whether wcrtomb return value is correct],
        [gl_cv_func_wcrtomb_retval],
        [
          dnl Initial guess, used when cross-compiling or when no suitable locale
          dnl is present.
changequote(,)dnl
          case "$host_os" in
            # Guess no on AIX 4, OSF/1, Solaris, native Windows.
            aix4* | osf* | solaris* | mingw*) gl_cv_func_wcrtomb_retval="guessing no" ;;
            # Guess yes otherwise.
            *)                                gl_cv_func_wcrtomb_retval="guessing yes" ;;
          esac
changequote([,])dnl
          if test $LOCALE_FR != none || test $LOCALE_FR_UTF8 != none || test $LOCALE_JA != none || test $LOCALE_ZH_CN != none; then
            AC_RUN_IFELSE(
              [AC_LANG_SOURCE([[
#include <locale.h>
#include <string.h>
#include <wchar.h>
#include <stdlib.h>
int main ()
{
  int result = 0;
  if (setlocale (LC_ALL, "$LOCALE_FR") != NULL)
    {
      if (wcrtomb (NULL, 0, NULL) != 1)
        result |= 1;
    }
  if (setlocale (LC_ALL, "$LOCALE_FR_UTF8") != NULL)
    {
      if (wcrtomb (NULL, 0, NULL) != 1)
        result |= 2;
      {
        wchar_t wc = (wchar_t) 0xBADFACE;
        if (mbtowc (&wc, "\303\274", 2) == 2)
          if (wcrtomb (NULL, wc, NULL) != 1)
            result |= 2;
      }
    }
  if (setlocale (LC_ALL, "$LOCALE_JA") != NULL)
    {
      if (wcrtomb (NULL, 0, NULL) != 1)
        result |= 4;
    }
  if (setlocale (LC_ALL, "$LOCALE_ZH_CN") != NULL)
    {
      if (wcrtomb (NULL, 0, NULL) != 1)
        result |= 8;
    }
  return result;
}]])],
              [gl_cv_func_wcrtomb_retval=yes],
              [gl_cv_func_wcrtomb_retval=no],
              [:])
          fi
        ])
      case "$gl_cv_func_wcrtomb_retval" in
        *yes) ;;
        *) AC_DEFINE([WCRTOMB_RETVAL_BUG], [1],
             [Define if the wcrtomb function has an incorrect return value.])
           REPLACE_WCRTOMB=1 ;;
      esac
    fi
  fi
])

# Prerequisites of lib/wcrtomb.c.
AC_DEFUN([gl_PREREQ_WCRTOMB], [
  :
])
