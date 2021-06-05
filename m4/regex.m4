# serial 73

# Copyright (C) 1996-2001, 2003-2021 Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

dnl Initially derived from code in GNU grep.
dnl Mostly written by Jim Meyering.

AC_PREREQ([2.50])

AC_DEFUN([gl_REGEX],
[
  AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
  AC_ARG_WITH([included-regex],
    [AS_HELP_STRING([--without-included-regex],
                    [don't compile regex; this is the default on systems
                     with recent-enough versions of the GNU C Library
                     (use with caution on other systems).])])

  case $with_included_regex in #(
  yes|no) ac_use_included_regex=$with_included_regex
        ;;
  '')
    # If the system regex support is good enough that it passes the
    # following run test, then default to *not* using the included regex.c.
    # If cross compiling, assume the test would fail and use the included
    # regex.c.
    AC_CHECK_DECLS_ONCE([alarm])
    AC_CHECK_HEADERS_ONCE([malloc.h])
    AC_CACHE_CHECK([for working re_compile_pattern],
                   [gl_cv_func_re_compile_pattern_working],
      [AC_RUN_IFELSE(
        [AC_LANG_PROGRAM(
          [[#include <regex.h>

            #include <locale.h>
            #include <limits.h>
            #include <string.h>

            #if defined M_CHECK_ACTION || HAVE_DECL_ALARM
            # include <signal.h>
            # include <unistd.h>
            #endif

            #if HAVE_MALLOC_H
            # include <malloc.h>
            #endif

            #ifdef M_CHECK_ACTION
            /* Exit with distinguishable exit code.  */
            static void sigabrt_no_core (int sig) { raise (SIGTERM); }
            #endif
          ]],
          [[int result = 0;
            static struct re_pattern_buffer regex;
            unsigned char folded_chars[UCHAR_MAX + 1];
            int i;
            const char *s;
            struct re_registers regs;

            /* Some builds of glibc go into an infinite loop on this
               test.  Use alarm to force death, and mallopt to avoid
               malloc recursion in diagnosing the corrupted heap. */
#if HAVE_DECL_ALARM
            signal (SIGALRM, SIG_DFL);
            alarm (2);
#endif
#ifdef M_CHECK_ACTION
            signal (SIGABRT, sigabrt_no_core);
            mallopt (M_CHECK_ACTION, 2);
#endif

            if (setlocale (LC_ALL, "en_US.UTF-8"))
              {
                {
                  /* https://sourceware.org/ml/libc-hacker/2006-09/msg00008.html
                     This test needs valgrind to catch the bug on Debian
                     GNU/Linux 3.1 x86, but it might catch the bug better
                     on other platforms and it shouldn't hurt to try the
                     test here.  */
                  static char const pat[] = "insert into";
                  static char const data[] =
                    "\xFF\0\x12\xA2\xAA\xC4\xB1,K\x12\xC4\xB1*\xACK";
                  re_set_syntax (RE_SYNTAX_GREP | RE_HAT_LISTS_NOT_NEWLINE
                                 | RE_ICASE);
                  memset (&regex, 0, sizeof regex);
                  s = re_compile_pattern (pat, sizeof pat - 1, &regex);
                  if (s)
                    result |= 1;
                  else
                    {
                      if (re_search (&regex, data, sizeof data - 1,
                                     0, sizeof data - 1, &regs)
                          != -1)
                        result |= 1;
                      regfree (&regex);
                    }
                }

                {
                  /* This test is from glibc bug 15078.
                     The test case is from Andreas Schwab in
                     <https://sourceware.org/ml/libc-alpha/2013-01/msg00967.html>.
                     */
                  static char const pat[] = "[^x]x";
                  static char const data[] =
                    /* <U1000><U103B><U103D><U1014><U103A><U102F><U1015><U103A> */
                    "\xe1\x80\x80"
                    "\xe1\x80\xbb"
                    "\xe1\x80\xbd"
                    "\xe1\x80\x94"
                    "\xe1\x80\xba"
                    "\xe1\x80\xaf"
                    "\xe1\x80\x95"
                    "\xe1\x80\xba"
                    "x";
                  re_set_syntax (0);
                  memset (&regex, 0, sizeof regex);
                  s = re_compile_pattern (pat, sizeof pat - 1, &regex);
                  if (s)
                    result |= 1;
                  else
                    {
                      i = re_search (&regex, data, sizeof data - 1,
                                     0, sizeof data - 1, 0);
                      if (i != 0 && i != 21)
                        result |= 1;
                      regfree (&regex);
                    }
                }

                if (! setlocale (LC_ALL, "C"))
                  return 1;
              }

            /* This test is from glibc bug 3957, reported by Andrew Mackey.  */
            re_set_syntax (RE_SYNTAX_EGREP | RE_HAT_LISTS_NOT_NEWLINE);
            memset (&regex, 0, sizeof regex);
            s = re_compile_pattern ("a[^x]b", 6, &regex);
            if (s)
              result |= 2;
            else
              {
                /* This should fail, but succeeds for glibc-2.5.  */
                if (re_search (&regex, "a\nb", 3, 0, 3, &regs) != -1)
                  result |= 2;
                regfree (&regex);
              }

            /* This regular expression is from Spencer ere test number 75
               in grep-2.3.  */
            re_set_syntax (RE_SYNTAX_POSIX_EGREP);
            memset (&regex, 0, sizeof regex);
            for (i = 0; i <= UCHAR_MAX; i++)
              folded_chars[i] = i;
            regex.translate = folded_chars;
            s = re_compile_pattern ("a[[:@:>@:]]b\n", 11, &regex);
            /* This should fail with _Invalid character class name_ error.  */
            if (!s)
              {
                result |= 4;
                regfree (&regex);
              }

            /* Ensure that [b-a] is diagnosed as invalid, when
               using RE_NO_EMPTY_RANGES. */
            re_set_syntax (RE_SYNTAX_POSIX_EGREP | RE_NO_EMPTY_RANGES);
            memset (&regex, 0, sizeof regex);
            s = re_compile_pattern ("a[b-a]", 6, &regex);
            if (s == 0)
              {
                result |= 8;
                regfree (&regex);
              }

            /* This should succeed, but does not for glibc-2.1.3.  */
            memset (&regex, 0, sizeof regex);
            s = re_compile_pattern ("{1", 2, &regex);
            if (s)
              result |= 8;
            else
              regfree (&regex);

            /* The following example is derived from a problem report
               against gawk from Jorge Stolfi <stolfi@ic.unicamp.br>.  */
            memset (&regex, 0, sizeof regex);
            s = re_compile_pattern ("[an\371]*n", 7, &regex);
            if (s)
              result |= 8;
            else
              {
                /* This should match, but does not for glibc-2.2.1.  */
                if (re_match (&regex, "an", 2, 0, &regs) != 2)
                  result |= 8;
                else
                  {
                    free (regs.start);
                    free (regs.end);
                  }
                regfree (&regex);
              }

            memset (&regex, 0, sizeof regex);
            s = re_compile_pattern ("x", 1, &regex);
            if (s)
              result |= 8;
            else
              {
                /* glibc-2.2.93 does not work with a negative RANGE argument.  */
                if (re_search (&regex, "wxy", 3, 2, -2, &regs) != 1)
                  result |= 8;
                else
                  {
                    free (regs.start);
                    free (regs.end);
                  }
                regfree (&regex);
              }

            /* The version of regex.c in older versions of gnulib
               ignored RE_ICASE.  Detect that problem too.  */
            re_set_syntax (RE_SYNTAX_EMACS | RE_ICASE);
            memset (&regex, 0, sizeof regex);
            s = re_compile_pattern ("x", 1, &regex);
            if (s)
              result |= 16;
            else
              {
                if (re_search (&regex, "WXY", 3, 0, 3, &regs) < 0)
                  result |= 16;
                else
                  {
                    free (regs.start);
                    free (regs.end);
                  }
                regfree (&regex);
              }

            /* Catch a bug reported by Vin Shelton in
               https://lists.gnu.org/r/bug-coreutils/2007-06/msg00089.html
               */
            re_set_syntax (RE_SYNTAX_POSIX_BASIC
                           & ~RE_CONTEXT_INVALID_DUP
                           & ~RE_NO_EMPTY_RANGES);
            memset (&regex, 0, sizeof regex);
            s = re_compile_pattern ("[[:alnum:]_-]\\\\+\$", 16, &regex);
            if (s)
              result |= 32;
            else
              regfree (&regex);

            /* REG_STARTEND was added to glibc on 2004-01-15.
               Reject older versions.  */
            if (! REG_STARTEND)
              result |= 64;

            /* Matching with the compiled form of this regexp would provoke
               an assertion failure prior to glibc-2.28:
                 regexec.c:1375: pop_fail_stack: Assertion 'num >= 0' failed
               With glibc-2.28, compilation fails and reports the invalid
               back reference.  */
            re_set_syntax (RE_SYNTAX_POSIX_EGREP);
            memset (&regex, 0, sizeof regex);
            s = re_compile_pattern ("0|()0|\\\\1|0", 10, &regex);
            if (!s)
              {
                memset (&regs, 0, sizeof regs);
                i = re_search (&regex, "x", 1, 0, 1, &regs);
                if (i != -1)
                  result |= 64;
                if (0 <= i)
                  {
                    free (regs.start);
                    free (regs.end);
                  }
                regfree (&regex);
              }
            else
              {
                if (strcmp (s, "Invalid back reference"))
                  result |= 64;
              }

            /* glibc bug 11053.  */
            re_set_syntax (RE_SYNTAX_POSIX_BASIC);
            memset (&regex, 0, sizeof regex);
            static char const pat_sub2[] = "\\\\(a*\\\\)*a*\\\\1";
            s = re_compile_pattern (pat_sub2, sizeof pat_sub2 - 1, &regex);
            if (s)
              result |= 64;
            else
              {
                memset (&regs, 0, sizeof regs);
                static char const data[] = "a";
                int datalen = sizeof data - 1;
                i = re_search (&regex, data, datalen, 0, datalen, &regs);
                if (i != 0)
                  result |= 64;
                else if (regs.num_regs < 2)
                  result |= 64;
                else if (! (regs.start[0] == 0 && regs.end[0] == 1))
                  result |= 64;
                else if (! (regs.start[1] == 0 && regs.end[1] == 0))
                  result |= 64;
                regfree (&regex);
                free (regs.start);
                free (regs.end);
              }

#if 0
            /* It would be nice to reject hosts whose regoff_t values are too
               narrow (including glibc on hosts with 64-bit ptrdiff_t and
               32-bit int), but we should wait until glibc implements this
               feature.  Otherwise, support for equivalence classes and
               multibyte collation symbols would always be broken except
               when compiling --without-included-regex.   */
            if (sizeof (regoff_t) < sizeof (ptrdiff_t)
                || sizeof (regoff_t) < sizeof (ssize_t))
              result |= 64;
#endif

            return result;
          ]])],
        [gl_cv_func_re_compile_pattern_working=yes],
        [gl_cv_func_re_compile_pattern_working=no],
        [case "$host_os" in
                   # Guess no on native Windows.
           mingw*) gl_cv_func_re_compile_pattern_working="guessing no" ;;
                   # Otherwise obey --enable-cross-guesses.
           *)      gl_cv_func_re_compile_pattern_working="$gl_cross_guess_normal" ;;
         esac
        ])
      ])
    case "$gl_cv_func_re_compile_pattern_working" in #(
      *yes) ac_use_included_regex=no;; #(
      *no) ac_use_included_regex=yes;;
    esac
    ;;
  *) AC_MSG_ERROR([Invalid value for --with-included-regex: $with_included_regex])
    ;;
  esac

  if test $ac_use_included_regex = yes; then
    AC_DEFINE([_REGEX_INCLUDE_LIMITS_H], [1],
      [Define if you want <regex.h> to include <limits.h>, so that it
       consistently overrides <limits.h>'s RE_DUP_MAX.])
    AC_DEFINE([_REGEX_LARGE_OFFSETS], [1],
      [Define if you want regoff_t to be at least as wide POSIX requires.])
    AC_DEFINE([re_syntax_options], [rpl_re_syntax_options],
      [Define to rpl_re_syntax_options if the replacement should be used.])
    AC_DEFINE([re_set_syntax], [rpl_re_set_syntax],
      [Define to rpl_re_set_syntax if the replacement should be used.])
    AC_DEFINE([re_compile_pattern], [rpl_re_compile_pattern],
      [Define to rpl_re_compile_pattern if the replacement should be used.])
    AC_DEFINE([re_compile_fastmap], [rpl_re_compile_fastmap],
      [Define to rpl_re_compile_fastmap if the replacement should be used.])
    AC_DEFINE([re_search], [rpl_re_search],
      [Define to rpl_re_search if the replacement should be used.])
    AC_DEFINE([re_search_2], [rpl_re_search_2],
      [Define to rpl_re_search_2 if the replacement should be used.])
    AC_DEFINE([re_match], [rpl_re_match],
      [Define to rpl_re_match if the replacement should be used.])
    AC_DEFINE([re_match_2], [rpl_re_match_2],
      [Define to rpl_re_match_2 if the replacement should be used.])
    AC_DEFINE([re_set_registers], [rpl_re_set_registers],
      [Define to rpl_re_set_registers if the replacement should be used.])
    AC_DEFINE([re_comp], [rpl_re_comp],
      [Define to rpl_re_comp if the replacement should be used.])
    AC_DEFINE([re_exec], [rpl_re_exec],
      [Define to rpl_re_exec if the replacement should be used.])
    AC_DEFINE([regcomp], [rpl_regcomp],
      [Define to rpl_regcomp if the replacement should be used.])
    AC_DEFINE([regexec], [rpl_regexec],
      [Define to rpl_regexec if the replacement should be used.])
    AC_DEFINE([regerror], [rpl_regerror],
      [Define to rpl_regerror if the replacement should be used.])
    AC_DEFINE([regfree], [rpl_regfree],
      [Define to rpl_regfree if the replacement should be used.])
  fi
])

# Prerequisites of lib/regex.c and lib/regex_internal.c.
AC_DEFUN([gl_PREREQ_REGEX],
[
  AC_REQUIRE([AC_USE_SYSTEM_EXTENSIONS])
  AC_REQUIRE([AC_C_INLINE])
  AC_REQUIRE([AC_C_RESTRICT])
  AC_REQUIRE([AC_TYPE_MBSTATE_T])
  AC_REQUIRE([gl_EEMALLOC])
  AC_CHECK_HEADERS([libintl.h])
  AC_CHECK_FUNCS_ONCE([isblank iswctype])
  AC_CHECK_DECLS([isblank], [], [], [[#include <ctype.h>]])
])
