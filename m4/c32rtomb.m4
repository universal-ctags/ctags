# c32rtomb.m4
# serial 10
dnl Copyright (C) 2020-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

AC_DEFUN([gl_FUNC_C32RTOMB],
[
  AC_REQUIRE([gl_UCHAR_H_DEFAULTS])
  AC_REQUIRE([AC_CANONICAL_HOST])

  AC_REQUIRE([gl_MBRTOC32_SANITYCHECK])
  AC_REQUIRE([gl_C32RTOMB_SANITYCHECK])

  AC_REQUIRE([gl_CHECK_FUNC_C32RTOMB])
  if test $gl_cv_func_c32rtomb = no; then
    HAVE_C32RTOMB=0
  else
    dnl When we override mbrtoc32, redefining the meaning of the char32_t
    dnl values, we need to override c32rtomb as well, for consistency.
    if test $HAVE_WORKING_MBRTOC32 = 0; then
      REPLACE_C32RTOMB=1
    fi
    AC_CACHE_CHECK([whether c32rtomb return value is correct],
      [gl_cv_func_c32rtomb_retval],
      [
        dnl Initial guess, used when cross-compiling.
changequote(,)dnl
        case "$host_os" in
          # Guess no on AIX.
          aix*) gl_cv_func_c32rtomb_retval="guessing no" ;;
          # Guess yes otherwise.
          *)    gl_cv_func_c32rtomb_retval="guessing yes" ;;
        esac
changequote([,])dnl
        AC_RUN_IFELSE(
          [AC_LANG_SOURCE([[
#include <stddef.h>
#ifdef __HAIKU__
 #include <stdint.h>
#endif
#include <uchar.h>
int main ()
{
  int result = 0;
  if (c32rtomb (NULL, 0, NULL) != 1)
    result |= 1;
  return result;
}]])],
          [gl_cv_func_c32rtomb_retval=yes],
          [gl_cv_func_c32rtomb_retval=no],
          [:])
      ])
    case "$gl_cv_func_c32rtomb_retval" in
      *yes) ;;
      *) AC_DEFINE([C32RTOMB_RETVAL_BUG], [1],
           [Define if the c32rtomb function has an incorrect return value.])
         REPLACE_C32RTOMB=1 ;;
    esac
    if test $HAVE_WORKING_C32RTOMB = 0; then
      REPLACE_C32RTOMB=1
    fi
  fi
])

AC_DEFUN([gl_CHECK_FUNC_C32RTOMB],
[
  dnl Cf. gl_CHECK_FUNCS_ANDROID
  AC_CHECK_DECL([c32rtomb], , ,
    [[#ifdef __HAIKU__
       #include <stdint.h>
      #endif
      #include <uchar.h>
    ]])
  if test $ac_cv_have_decl_c32rtomb = yes; then
    dnl We can't use AC_CHECK_FUNC here, because c32rtomb() is defined as a
    dnl static inline function on Haiku 2020.
    AC_CACHE_CHECK([for c32rtomb], [gl_cv_func_c32rtomb],
      [AC_LINK_IFELSE(
         [AC_LANG_PROGRAM(
            [[#include <stdlib.h>
              #ifdef __HAIKU__
               #include <stdint.h>
              #endif
              #include <uchar.h>
            ]],
            [[char buf[8];
              return c32rtomb (buf, 0, NULL) == 0;
            ]])
         ],
         [gl_cv_func_c32rtomb=yes],
         [gl_cv_func_c32rtomb=no])
      ])
  else
    gl_cv_func_c32rtomb=no
  fi
])

dnl Test whether c32rtomb works not worse than wcrtomb.
dnl Result is HAVE_WORKING_C32RTOMB.

AC_DEFUN([gl_C32RTOMB_SANITYCHECK],
[
  AC_REQUIRE([AC_PROG_CC])
  AC_REQUIRE([gl_TYPE_CHAR32_T])
  AC_REQUIRE([gl_CHECK_FUNC_C32RTOMB])
  AC_REQUIRE([gt_LOCALE_ZH_CN])
  AC_REQUIRE([AC_CANONICAL_HOST])
  if test $GNULIBHEADERS_OVERRIDE_CHAR32_T = 1 || test $gl_cv_func_c32rtomb = no; then
    HAVE_WORKING_C32RTOMB=0
  else
    AC_CACHE_CHECK([whether c32rtomb works as well as wcrtomb],
      [gl_cv_func_c32rtomb_sanitycheck],
      [
        dnl Initial guess, used when cross-compiling or when no suitable locale
        dnl is present.
changequote(,)dnl
        case "$host_os" in
          # Guess no on Solaris derivatives.
          solaris*)
            if test -f /etc/release && grep 'Oracle Solaris' /etc/release >/dev/null; then
              gl_cv_func_c32rtomb_sanitycheck="guessing yes"
            else
              gl_cv_func_c32rtomb_sanitycheck="guessing no"
            fi
            ;;
          # Guess yes otherwise.
          *)
            gl_cv_func_c32rtomb_sanitycheck="guessing yes"
            ;;
        esac
changequote([,])dnl
        if test $LOCALE_ZH_CN != none; then
          AC_RUN_IFELSE(
            [AC_LANG_SOURCE([[
#include <locale.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>
#ifdef __HAIKU__
 #include <stdint.h>
#endif
#include <uchar.h>
int main ()
{
  int result = 0;
  /* This fails on Solaris 11 OmniOS:
     c32rtomb returns (size_t)-1.
     wcrtomb returns 4 (correct).  */
  if (strcmp ("$LOCALE_ZH_CN", "none") != 0
      && setlocale (LC_ALL, "$LOCALE_ZH_CN") != NULL)
    {
      mbstate_t state;
      wchar_t wc = (wchar_t) {0xBADFACE};
      char buf[16];
      memset (&state, '\0', sizeof (mbstate_t));
      if (mbrtowc (&wc, "\201\060\211\070", 4, &state) == 4
          && wcrtomb (buf, wc, NULL) == 4
          && memcmp (buf, "\201\060\211\070", 4) == 0)
        {
          char32_t c32 = (wchar_t) {0xBADFACE};
          memset (&state, '\0', sizeof (mbstate_t));
          if (mbrtoc32 (&c32, "\201\060\211\070", 4, &state) == 4
              && c32rtomb (buf, c32, NULL) != 4)
            result |= 1;
        }
    }
  return result;
}]])],
            [gl_cv_func_c32rtomb_sanitycheck=yes],
            [gl_cv_func_c32rtomb_sanitycheck=no],
            [:])
        fi
      ])
    case "$gl_cv_func_c32rtomb_sanitycheck" in
      *yes)
        HAVE_WORKING_C32RTOMB=1
        AC_DEFINE([HAVE_WORKING_C32RTOMB], [1],
          [Define if the c32rtomb function basically works.])
        ;;
      *) HAVE_WORKING_C32RTOMB=0 ;;
    esac
  fi
  AC_SUBST([HAVE_WORKING_C32RTOMB])
])
