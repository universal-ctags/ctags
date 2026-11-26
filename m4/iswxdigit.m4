# iswxdigit.m4
# serial 11
dnl Copyright (C) 2020-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

AC_DEFUN([gl_FUNC_ISWXDIGIT],
[
  AC_REQUIRE([gl_WCTYPE_H_DEFAULTS])
  AC_REQUIRE([gl_WCTYPE_H])
  AC_REQUIRE([gt_LOCALE_JA])
  AC_REQUIRE([gt_LOCALE_EN_UTF8])
  AC_REQUIRE([gt_LOCALE_ZH_CN])
  AC_REQUIRE([AC_CANONICAL_HOST])

  if test $HAVE_ISWCNTRL = 0 || test $REPLACE_ISWCNTRL = 1; then
    dnl <wctype.h> redefines iswxdigit already.
    REPLACE_ISWXDIGIT="$REPLACE_ISWCNTRL"
  else
    AC_CACHE_CHECK([whether iswxdigit is ISO C compliant],
      [gl_cv_func_iswxdigit_works],
      [
       dnl Initial guess, used when cross-compiling or when no suitable locale
       dnl is present.
changequote(,)dnl
       case "$host_os" in
         # Guess no on FreeBSD, NetBSD, OpenBSD, Solaris, native Windows, Haiku, Android.
         freebsd* | dragonfly* | netbsd* | openbsd* | solaris* | mingw* | windows* | haiku* | *-android*)
           gl_cv_func_iswxdigit_works="guessing no" ;;
         # Guess yes otherwise.
         *) gl_cv_func_iswxdigit_works="guessing yes" ;;
       esac
changequote([,])dnl
       if test $LOCALE_JA != none || test "$LOCALE_EN_UTF8" != none || test $LOCALE_ZH_CN != none; then
         AC_RUN_IFELSE(
           [AC_LANG_SOURCE([[
#include <locale.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>
#include <wctype.h>

/* Returns the value of iswxdigit for the multibyte character s[0..n-1].  */
static int
for_character (const char *s, size_t n)
{
  mbstate_t state;
  wchar_t wc;
  size_t ret;

  memset (&state, '\0', sizeof (mbstate_t));
  wc = (wchar_t) {0xBADFACE};
  ret = mbrtowc (&wc, s, n, &state);
  if (ret != n)
    abort ();

  return iswxdigit (wc);
}

int
main (int argc, char *argv[])
{
  int is;
  int result = 0;

  if (strcmp ("$LOCALE_JA", "none") != 0
      && setlocale (LC_ALL, "$LOCALE_JA") != NULL)
    {
      /* This fails on NetBSD 10.0.  */
      /* U+FF21 FULLWIDTH LATIN CAPITAL LETTER A */
      is = for_character ("\243\301", 2);
      if (!(is == 0))
        result |= 1;
    }
  if (strcmp ("$LOCALE_EN_UTF8", "none") != 0
      && setlocale (LC_ALL, "$LOCALE_EN_UTF8") != NULL)
    {
      /* This fails on FreeBSD 13.0, Haiku, Android.  */
      /* U+0663 ARABIC-INDIC DIGIT THREE */
      is = for_character ("\331\243", 2);
      if (!(is == 0))
        result |= 2;
      /* This fails on NetBSD 10.0, OpenBSD 7.5, MSVC 14, Haiku, Android.  */
      /* U+FF21 FULLWIDTH LATIN CAPITAL LETTER A */
      is = for_character ("\357\274\241", 3);
      if (!(is == 0))
        result |= 4;
    }
  if (strcmp ("$LOCALE_ZH_CN", "none") != 0
      && setlocale (LC_ALL, "$LOCALE_ZH_CN") != NULL)
    {
      /* This fails on Solaris 10, Solaris 11.4.  */
      /* U+FF11 FULLWIDTH DIGIT ONE */
      is = for_character ("\243\261", 2);
      if (!(is == 0))
        result |= 8;
    }
  return result;
}]])],
           [gl_cv_func_iswxdigit_works=yes],
           [gl_cv_func_iswxdigit_works=no],
           [:])
       fi
      ])
    case "$gl_cv_func_iswxdigit_works" in
      *yes) ;;
      *) REPLACE_ISWXDIGIT=1 ;;
    esac
  fi
])
