# btowc.m4
# serial 16
dnl Copyright (C) 2008-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

AC_DEFUN([gl_FUNC_BTOWC],
[
  AC_REQUIRE([gl_WCHAR_H_DEFAULTS])
  AC_REQUIRE([gt_TYPE_WINT_T])

  dnl Check whether <wchar.h> is usable at all, first. Otherwise the test
  dnl program below may lead to an endless loop. See
  dnl <https://gcc.gnu.org/PR42440>.
  AC_REQUIRE([gl_WCHAR_H_INLINE_OK])

  AC_CHECK_FUNCS_ONCE([btowc])
  if test $ac_cv_func_btowc = no; then
    HAVE_BTOWC=0
  else

    AC_REQUIRE([AC_PROG_CC])
    AC_REQUIRE([gt_LOCALE_FR])
    AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles

    dnl Cygwin 1.7.2 btowc('\0') is WEOF, not 0.
    AC_CACHE_CHECK([whether btowc(0) is correct],
      [gl_cv_func_btowc_nul],
      [
        AC_RUN_IFELSE(
          [AC_LANG_SOURCE([[
#include <wchar.h>
int main ()
{
  if (btowc ('\0') != 0)
    return 1;
  return 0;
}]])],
          [gl_cv_func_btowc_nul=yes],
          [gl_cv_func_btowc_nul=no],
          [
changequote(,)dnl
           case "$host_os" in
                                # Guess no on Cygwin.
             cygwin*)           gl_cv_func_btowc_nul="guessing no" ;;
                                # Guess yes on native Windows.
             mingw* | windows*) gl_cv_func_btowc_nul="guessing yes" ;;
                                # Guess yes otherwise.
             *)                 gl_cv_func_btowc_nul="guessing yes" ;;
           esac
changequote([,])dnl
          ])
      ])

    dnl On mingw, in the C locale, btowc is inconsistent with mbrtowc:
    dnl mbrtowc avoids calling MultiByteToWideChar when MB_CUR_MAX is 1 and
    dnl ___lc_codepage_func() is 0, but btowc is lacking this special case.
    AC_CHECK_FUNCS_ONCE([mbrtowc])
    AC_CACHE_CHECK([whether btowc is consistent with mbrtowc in the C locale],
      [gl_cv_func_btowc_consistent],
      [
        AC_RUN_IFELSE(
          [AC_LANG_SOURCE([[
#include <stdlib.h>
#include <string.h>
#include <wchar.h>
int main ()
{
#if HAVE_MBRTOWC
  wint_t wc1 = btowc (0x80);
  wchar_t wc2 = (wchar_t) 0xbadface;
  char buf[1] = { 0x80 };
  mbstate_t state;
  memset (&state, 0, sizeof (mbstate_t));
  if (mbrtowc (&wc2, buf, 1, &state) != 1 || wc1 != wc2)
    return 1;
#endif
  return 0;
}]])],
          [gl_cv_func_btowc_consistent=yes],
          [gl_cv_func_btowc_consistent=no],
          [case "$host_os" in
               # Guess no on mingw.
             mingw* | windows*)
               AC_EGREP_CPP([Problem], [
#ifdef __MINGW32__
 Problem
#endif
                 ],
                 [gl_cv_func_btowc_consistent="guessing no"],
                 [gl_cv_func_btowc_consistent="guessing yes"])
               ;;
               # Guess yes otherwise.
             *) gl_cv_func_btowc_consistent="guessing yes" ;;
           esac
          ])
      ])

    if test $GNULIBHEADERS_OVERRIDE_WINT_T = 1; then
      dnl On mingw/ucrt, we override the return type of btowc().
      dnl While the original wint_t (= unsigned short) and the overridden wint_t
      dnl (= unsigned int) are equivalent in function parameters, this is not
      dnl the case for function return types.
      REPLACE_BTOWC=1
    fi
    case "$gl_cv_func_btowc_nul" in
      *yes) ;;
      *) REPLACE_BTOWC=1 ;;
    esac
    case "$gl_cv_func_btowc_consistent" in
      *yes) ;;
      *) REPLACE_BTOWC=1 ;;
    esac
    if test $REPLACE_BTOWC = 0; then
      gl_MBRTOWC_C_LOCALE
      case "$gl_cv_func_mbrtowc_C_locale_sans_EILSEQ" in
        *yes) ;;
        *) REPLACE_BTOWC=1 ;;
      esac
    fi
  fi
])

# Prerequisites of lib/btowc.c.
AC_DEFUN([gl_PREREQ_BTOWC], [
  :
  AC_CHECK_FUNCS_ONCE([mbrtowc])
])
