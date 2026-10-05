# locale-en.m4
# serial 1
dnl Copyright (C) 2003-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

dnl From Bruno Haible.

dnl Determine the name of an English (or American English) locale with
dnl UTF-8 encoding.
AC_DEFUN_ONCE([gt_LOCALE_EN_UTF8],
[
  AC_REQUIRE([AC_CANONICAL_HOST])
  AC_REQUIRE([AM_LANGINFO_CODESET])
  AC_CACHE_CHECK([for an english Unicode locale], [gt_cv_locale_en_utf8], [
    case "$host_os" in
      *-musl* | midipix*)
        dnl On musl libc, all kinds of ll_CC.UTF-8 locales exist, even without
        dnl any locale file on disk. But they are effectively equivalent to the
        dnl C.UTF-8 locale, except for locale categories (such as LC_MESSAGES)
        dnl for which localizations (.mo files) have been installed.
        gt_cv_locale_en_utf8=en_US.UTF-8
        ;;
      *)
        AC_LANG_CONFTEST([AC_LANG_SOURCE([[
#include <locale.h>
#include <time.h>
#if HAVE_LANGINFO_CODESET
# include <langinfo.h>
#endif
#include <stdlib.h>
#include <string.h>
struct tm t;
char buf[16];
int main () {
  /* On BeOS and Haiku, locales are not implemented in libc.  Rather, libintl
     imitates locale dependent behaviour by looking at the environment
     variables, and all locales use the UTF-8 encoding.  */
#if !(defined __BEOS__ || defined __HAIKU__)
  /* Check whether the given locale name is recognized by the system.  */
# if defined _WIN32 && !defined __CYGWIN__
  /* On native Windows, setlocale(category, "") looks at the system settings,
     not at the environment variables.  Also, when an encoding suffix such
     as ".65001" or ".54936" is specified, it succeeds but sets the LC_CTYPE
     category of the locale to "C".  */
  if (setlocale (LC_ALL, getenv ("LC_ALL")) == NULL
      || strcmp (setlocale (LC_CTYPE, NULL), "C") == 0)
    return 1;
# else
  if (setlocale (LC_ALL, "") == NULL) return 1;
# endif
  /* Check whether nl_langinfo(CODESET) is "UTF-8" or equivalent.  */
# if HAVE_LANGINFO_CODESET
  {
    const char *cs = nl_langinfo (CODESET);
    if (!(strcmp (cs, "UTF-8") == 0 || strcmp (cs, "UTF8") == 0
          || strcmp (cs, "utf-8") == 0 || strcmp (cs, "utf8") == 0))
      return 1;
  }
# endif
# ifdef __CYGWIN__
  /* On Cygwin, avoid locale names without encoding suffix, because the
     locale_charset() function relies on the encoding suffix.  Note that
     LC_ALL is set on the command line.  */
  if (strchr (getenv ("LC_ALL"), '.') == NULL) return 1;
# endif
  /* Check the third month name.  */
  t.tm_year = 1975 - 1900; t.tm_mon = 3 - 1; t.tm_mday = 24;
  if (strftime (buf, sizeof (buf), "%B", &t) < 5 || strcmp (buf, "March") != 0)
    return 1;
#endif
#if !defined __BIONIC__ /* Bionic libc's 'struct lconv' is just a dummy.  */
  /* Check whether the decimal separator is a dot.  */
  if (localeconv () ->decimal_point[0] != '.') return 1;
#endif
  return 0;
}
          ]])])
        if AC_TRY_EVAL([ac_link]) && test -s conftest$ac_exeext; then
          case "$host_os" in
            # Handle native Windows specially, because there setlocale() interprets
            # "ar" or "ara" as "Arabic" or "Arabic_Saudi Arabia.1256",
            # "en" or "eng" as "English" or "English_United States.1252",
            # "fr" or "fra" as "French" or "French_France.1252",
            # "ge"(!) or "deu"(!) as "German" or "German_Germany.1252",
            # "ja" or "jpn" as "Japanese" or "Japanese_Japan.932",
            # and similar.
            mingw* | windows*)
              # Test for the hypothetical native Windows locale name.
              if (LC_ALL='English_United States.65001' LC_TIME= LC_CTYPE= ./conftest; exit) 2>/dev/null; then
                gt_cv_locale_en_utf8='English_United States.65001'
              else
                # None found.
                gt_cv_locale_en_utf8=none
              fi
              ;;
            *)
              # Setting LC_ALL is not enough. Need to set LC_TIME to empty, because
              # otherwise on Mac OS X 10.3.5 the LC_TIME=C from the beginning of the
              # configure script would override the LC_ALL setting. Likewise for
              # LC_CTYPE, which is also set at the beginning of the configure script.
              # Test for the locale name with explicit encoding suffix first
              # (this is necessary on Haiku).
              if (LC_ALL=en_US.UTF-8 LC_TIME= LC_CTYPE= ./conftest; exit) 2>/dev/null; then
                gt_cv_locale_en_utf8=en_US.UTF-8
              else
                # Test for the locale name without encoding suffix.
                if (LC_ALL=en_US LC_TIME= LC_CTYPE= ./conftest; exit) 2>/dev/null; then
                  gt_cv_locale_en_utf8=en_US
                else
                  # Test for the Solaris 10 locale name.
                  if (LC_ALL=en.UTF-8 LC_TIME= LC_CTYPE= ./conftest; exit) 2>/dev/null; then
                    gt_cv_locale_en_utf8=en.UTF-8
                  else
                    # None found.
                    gt_cv_locale_en_utf8=none
                  fi
                fi
              fi
              ;;
          esac
        fi
        rm -fr conftest*
        ;;
    esac
  ])
  LOCALE_EN_UTF8="$gt_cv_locale_en_utf8"
  case "$LOCALE_EN_UTF8" in #(
    '' | *[[\"\$\'*@<:@]]*)
      dnl The empty value occurs when the conftest.c program above could not
      dnl be compiled.  The other values might cause trouble with sh or make.
      AC_MSG_WARN([invalid locale "$LOCALE_EN_UTF8"; assuming "none"])
      LOCALE_EN_UTF8=none;;
  esac
  AC_SUBST([LOCALE_EN_UTF8])
])
