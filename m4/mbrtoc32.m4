# mbrtoc32.m4
# serial 25
dnl Copyright (C) 2014-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

AC_DEFUN([gl_FUNC_MBRTOC32],
[
  AC_REQUIRE([gl_UCHAR_H_DEFAULTS])

  AC_REQUIRE([AC_TYPE_MBSTATE_T])
  dnl Determine REPLACE_MBSTATE_T, from which GNULIB_defined_mbstate_t is
  dnl determined.  It describes how our overridden mbrtowc is implemented.
  dnl We then implement mbrtoc32 accordingly.
  AC_REQUIRE([gl_MBSTATE_T_BROKEN])

  AC_REQUIRE([gl_TYPE_CHAR32_T])
  AC_REQUIRE([gl_MBRTOC32_SANITYCHECK])

  AC_REQUIRE([gl_CHECK_FUNC_MBRTOC32])
  if test $gl_cv_func_mbrtoc32 = no; then
    HAVE_MBRTOC32=0
  else
    if test $GNULIBHEADERS_OVERRIDE_CHAR32_T = 1 || test $REPLACE_MBSTATE_T = 1; then
      REPLACE_MBRTOC32=1
    else
      gl_MBRTOC32_EMPTY_INPUT
      gl_MBRTOC32_C_LOCALE
      gl_MBRTOC32_UTF8_LOCALE
      case "$gl_cv_func_mbrtoc32_empty_input" in
        *yes) ;;
        *) AC_DEFINE([MBRTOC32_EMPTY_INPUT_BUG], [1],
             [Define if the mbrtoc32 function does not return (size_t) -2 for empty input.])
           REPLACE_MBRTOC32=1
           ;;
      esac
      case "$gl_cv_func_mbrtoc32_C_locale_sans_EILSEQ" in
        *yes) ;;
        *) AC_DEFINE([MBRTOC32_IN_C_LOCALE_MAYBE_EILSEQ], [1],
             [Define if the mbrtoc32 function may signal encoding errors in the C locale.])
           REPLACE_MBRTOC32=1
           ;;
      esac
      case "$gl_cv_func_mbrtoc32_utf8_locale_works" in
        *yes) ;;
        *) AC_DEFINE([MBRTOC32_MULTIBYTE_LOCALE_BUG], [1],
             [Define if the mbrtoc32 function does not accept the input bytes one-by-one.])
           REPLACE_MBRTOC32=1
           dnl Our replacement mbrtoc32 can handle UTF-8, but not GB18030.
           LOCALE_ZH_CN=none
           ;;
      esac
      m4_ifdef([gl_FUNC_MBRTOC32_REGULAR], [
        dnl The package requests a regular mbrtoc32 function.
        dnl glibc's mbrtoc32 function is not regular,
        dnl due to the zh_HK.BIG5-HKSCS locale, see
        dnl https://sourceware.org/bugzilla/show_bug.cgi?id=25734
        dnl https://sourceware.org/bugzilla/show_bug.cgi?id=30611
        AC_CACHE_CHECK([whether mbrtoc32 is regular],
          [gl_cv_func_mbrtoc32_regular],
          [AC_REQUIRE([AC_CANONICAL_HOST])
           gl_cv_func_mbrtoc32_regular="guessing yes"
           case "$host_os" in
             *-gnu* | gnu*)
               AC_EGREP_CPP([Unlucky], [
                 #include <features.h>
                 #if defined __GNU_LIBRARY__ && __GLIBC__ >= 2
                   Unlucky GNU user
                 #endif
                 ],
                 [gl_cv_func_mbrtoc32_regular="guessing no"],
                 [])
               ;;
           esac
          ])
      ], [
        dnl The package does not request a regular mbrtoc32 function.
        gl_cv_func_mbrtoc32_regular=irrelevant
      ])
      case "$gl_cv_func_mbrtoc32_regular" in
        *no) REPLACE_MBRTOC32=1 ;;
      esac
    fi
    if test $HAVE_WORKING_MBRTOC32 = 0; then
      REPLACE_MBRTOC32=1
    fi
  fi
])

AC_DEFUN([gl_CHECK_FUNC_MBRTOC32],
[
  dnl Cf. gl_CHECK_FUNCS_ANDROID
  AC_CHECK_DECL([mbrtoc32], , ,
    [[#ifdef __HAIKU__
       #include <stdint.h>
      #endif
      #include <uchar.h>
    ]])
  if test $ac_cv_have_decl_mbrtoc32 = yes; then
    dnl We can't use AC_CHECK_FUNC here, because mbrtoc32() is defined as a
    dnl static inline function on Haiku 2020.
    AC_CACHE_CHECK([for mbrtoc32], [gl_cv_func_mbrtoc32],
      [AC_LINK_IFELSE(
         [AC_LANG_PROGRAM(
            [[#include <stdlib.h>
              #ifdef __HAIKU__
               #include <stdint.h>
              #endif
              #include <uchar.h>
            ]],
            [[char32_t c;
              return mbrtoc32 (&c, "", 1, NULL) == 0;
            ]])
         ],
         [gl_cv_func_mbrtoc32=yes],
         [gl_cv_func_mbrtoc32=no])
      ])
  else
    gl_cv_func_mbrtoc32=no
  fi
])

dnl Test whether mbrtoc32 returns the correct value on empty input.

AC_DEFUN([gl_MBRTOC32_EMPTY_INPUT],
[
  AC_REQUIRE([AC_PROG_CC])
  AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
  AC_CACHE_CHECK([whether mbrtoc32 works on empty input],
    [gl_cv_func_mbrtoc32_empty_input],
    [
      AC_RUN_IFELSE(
        [AC_LANG_SOURCE([[
           #ifdef __HAIKU__
            #include <stdint.h>
           #endif
           #include <uchar.h>
           static char32_t wc;
           static mbstate_t mbs;
           int
           main (void)
           {
             return mbrtoc32 (&wc, "", 0, &mbs) != (size_t) -2;
           }]])],
        [gl_cv_func_mbrtoc32_empty_input=yes],
        [gl_cv_func_mbrtoc32_empty_input=no],
        [case "$host_os" in
                              # Guess no on glibc systems.
           *-gnu* | gnu*)     gl_cv_func_mbrtoc32_empty_input="guessing no" ;;
                              # Guess no on Android.
           linux*-android*)   gl_cv_func_mbrtoc32_empty_input="guessing no" ;;
                              # Guess no on native Windows.
           mingw* | windows*) gl_cv_func_mbrtoc32_empty_input="guessing no" ;;
           *)                 gl_cv_func_mbrtoc32_empty_input="guessing yes" ;;
         esac
        ])
    ])
])

dnl <https://pubs.opengroup.org/onlinepubs/9699919799/functions/mbrtowc.html>
dnl POSIX:2018 says regarding mbrtowc: "In the POSIX locale an [EILSEQ] error
dnl cannot occur since all byte values are valid characters."  It is reasonable
dnl to expect mbrtoc32 to behave in the same way.

AC_DEFUN([gl_MBRTOC32_C_LOCALE],
[
  AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
  AC_CACHE_CHECK([whether the C locale is free of encoding errors],
    [gl_cv_func_mbrtoc32_C_locale_sans_EILSEQ],
    [AC_RUN_IFELSE(
       [AC_LANG_PROGRAM(
          [[#include <limits.h>
            #include <locale.h>
            #ifdef __HAIKU__
             #include <stdint.h>
            #endif
            #include <uchar.h>
          ]], [[
            int i;
            const char *locale = setlocale (LC_ALL, "C");
            if (! locale)
              return 2;
            for (i = CHAR_MIN; i <= CHAR_MAX; i++)
              {
                char c = i;
                char32_t wc;
                mbstate_t mbs = { 0, };
                size_t ss = mbrtoc32 (&wc, &c, 1, &mbs);
                if (1 < ss)
                  return 3;
              }
            return 0;
          ]])],
       [gl_cv_func_mbrtoc32_C_locale_sans_EILSEQ=yes],
       [gl_cv_func_mbrtoc32_C_locale_sans_EILSEQ=no],
       [case "$host_os" in
                             # Guess yes on native Windows.
          mingw* | windows*) gl_cv_func_mbrtoc32_C_locale_sans_EILSEQ="guessing yes" ;;
          *)                 gl_cv_func_mbrtoc32_C_locale_sans_EILSEQ="$gl_cross_guess_normal" ;;
        esac
       ])
    ])
])

dnl Test whether mbrtoc32 works when it's fed the bytes one-by-one in an UTF-8
dnl locale.

AC_DEFUN([gl_MBRTOC32_UTF8_LOCALE],
[
  AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
  AC_CACHE_CHECK([whether mbrtoc32 works in an UTF-8 locale],
    [gl_cv_func_mbrtoc32_utf8_locale_works],
    [AC_RUN_IFELSE(
       [AC_LANG_PROGRAM(
          [[#include <locale.h>
            #ifdef __HAIKU__
             #include <stdint.h>
            #endif
            #include <uchar.h>
          ]], [[
            const char *locale = setlocale (LC_ALL, "en_US.UTF-8");
            if (locale)
              {
                /* This test fails on Cygwin 3.5.3.  */
                mbstate_t state = { 0, };
                char32_t uc = 0xDEADBEEF;
                /* \360\237\220\203 = U+0001F403 */
                if (mbrtoc32 (&uc, "\360", 1, &state) != (size_t)-2)
                  return 1;
                if (mbrtoc32 (&uc, "\237", 1, &state) != (size_t)-2)
                  return 2;
                if (mbrtoc32 (&uc, "\220", 1, &state) != (size_t)-2)
                  return 3;
                if (mbrtoc32 (&uc, "\203", 1, &state) != 1)
                  return 4;
                if (uc != 0x0001F403)
                  return 5;
              }
            return 0;
          ]])],
       [gl_cv_func_mbrtoc32_utf8_locale_works=yes],
       [gl_cv_func_mbrtoc32_utf8_locale_works=no],
       [case "$host_os" in
                   # Guess no on Cygwin.
          cygwin*) gl_cv_func_mbrtoc32_utf8_locale_works="guessing no" ;;
          *)       gl_cv_func_mbrtoc32_utf8_locale_works="$gl_cross_guess_normal" ;;
        esac
       ])
    ])
])

dnl Test whether mbrtoc32 works not worse than mbrtowc.
dnl Result is HAVE_WORKING_MBRTOC32.

AC_DEFUN([gl_MBRTOC32_SANITYCHECK],
[
  AC_REQUIRE([AC_PROG_CC])
  AC_REQUIRE([gl_TYPE_CHAR32_T])
  AC_REQUIRE([gl_CHECK_FUNC_MBRTOC32])
  AC_REQUIRE([gt_LOCALE_FR])
  AC_REQUIRE([gt_LOCALE_ZH_CN])
  AC_REQUIRE([AC_CANONICAL_HOST])
  if test $GNULIBHEADERS_OVERRIDE_CHAR32_T = 1 || test $gl_cv_func_mbrtoc32 = no; then
    HAVE_WORKING_MBRTOC32=0
  else
    AC_CACHE_CHECK([whether mbrtoc32 works as well as mbrtowc],
      [gl_cv_func_mbrtoc32_sanitycheck],
      [
        dnl Initial guess, used when cross-compiling or when no suitable locale
        dnl is present.
changequote(,)dnl
        case "$host_os" in
          # Guess no on FreeBSD, Solaris, native Windows.
          freebsd* | midnightbsd* | solaris* | mingw* | windows*)
            gl_cv_func_mbrtoc32_sanitycheck="guessing no"
            ;;
          # Guess yes otherwise.
          *)
            gl_cv_func_mbrtoc32_sanitycheck="guessing yes"
            ;;
        esac
changequote([,])dnl
        if test $LOCALE_FR != none || test $LOCALE_ZH_CN != none; then
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
  /* This fails on native Windows:
     mbrtoc32 returns (size_t)-1.
     mbrtowc returns 1 (correct).  */
  if (strcmp ("$LOCALE_FR", "none") != 0
      && setlocale (LC_ALL, "$LOCALE_FR") != NULL)
    {
      mbstate_t state;
      wchar_t wc = (wchar_t) {0xBADFACE};
      memset (&state, '\0', sizeof (mbstate_t));
      if (mbrtowc (&wc, "\374", 1, &state) == 1)
        {
          char32_t c32 = (wchar_t) {0xBADFACE};
          memset (&state, '\0', sizeof (mbstate_t));
          if (mbrtoc32 (&c32, "\374", 1, &state) != 1)
            result |= 1;
        }
    }
  /* This fails on FreeBSD 13.0 and Solaris 11.4:
     mbrtoc32 returns (size_t)-2 or (size_t)-1.
     mbrtowc returns 4 (correct).  */
  if (strcmp ("$LOCALE_ZH_CN", "none") != 0
      && setlocale (LC_ALL, "$LOCALE_ZH_CN") != NULL)
    {
      mbstate_t state;
      wchar_t wc = (wchar_t) {0xBADFACE};
      memset (&state, '\0', sizeof (mbstate_t));
      if (mbrtowc (&wc, "\224\071\375\067", 4, &state) == 4)
        {
          char32_t c32 = (wchar_t) {0xBADFACE};
          memset (&state, '\0', sizeof (mbstate_t));
          if (mbrtoc32 (&c32, "\224\071\375\067", 4, &state) != 4)
            result |= 2;
        }
    }
  return result;
}]])],
            [gl_cv_func_mbrtoc32_sanitycheck=yes],
            [gl_cv_func_mbrtoc32_sanitycheck=no],
            [:])
        fi
      ])
    case "$gl_cv_func_mbrtoc32_sanitycheck" in
      *yes)
        HAVE_WORKING_MBRTOC32=1
        AC_DEFINE([HAVE_WORKING_MBRTOC32], [1],
          [Define if the mbrtoc32 function basically works.])
        ;;
      *) HAVE_WORKING_MBRTOC32=0 ;;
    esac
  fi
  AC_SUBST([HAVE_WORKING_MBRTOC32])
])

# Prerequisites of lib/mbrtoc32.c and lib/lc-charset-dispatch.c.
AC_DEFUN([gl_PREREQ_MBRTOC32], [
  AC_REQUIRE([gl_C32RTOMB_SANITYCHECK])
  :
])
