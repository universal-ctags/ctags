# Check for fnmatch - serial 15.  -*- coding: utf-8 -*-

# Copyright (C) 2000-2007, 2009-2021 Free Software Foundation, Inc.
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# Autoconf defines AC_FUNC_FNMATCH, but that is obsolescent.
# New applications should use the macros below instead.

# Request a POSIX compliant fnmatch function.
AC_DEFUN([gl_FUNC_FNMATCH_POSIX],
[
  m4_divert_text([DEFAULTS], [gl_fnmatch_required=POSIX])

  AC_REQUIRE([gl_FNMATCH_H])
  AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
  gl_fnmatch_required_lowercase=`
    echo $gl_fnmatch_required | LC_ALL=C tr '[[A-Z]]' '[[a-z]]'
  `
  AC_CHECK_FUNCS_ONCE([fnmatch])
  if test $ac_cv_func_fnmatch = no; then
    HAVE_FNMATCH=0
  else
    gl_fnmatch_cache_var="gl_cv_func_fnmatch_${gl_fnmatch_required_lowercase}"
    AC_CACHE_CHECK([for working $gl_fnmatch_required fnmatch],
      [$gl_fnmatch_cache_var],
      [dnl Some versions of Solaris, SCO, and the GNU C Library
       dnl have a broken or incompatible fnmatch.
       dnl So we run a test program.  If we are cross-compiling, take no chance.
       dnl Thanks to John Oleynick, François Pinard, and Paul Eggert for this
       dnl test.
       if test $gl_fnmatch_required = GNU; then
         gl_fnmatch_gnu_start=
         gl_fnmatch_gnu_end=
       else
         gl_fnmatch_gnu_start='#if 0'
         gl_fnmatch_gnu_end='#endif'
       fi
       AC_RUN_IFELSE(
         [AC_LANG_PROGRAM(
            [[#include <fnmatch.h>
              static int
              y (char const *pattern, char const *string, int flags)
              {
                return fnmatch (pattern, string, flags) == 0;
              }
              static int
              n (char const *pattern, char const *string, int flags)
              {
                return fnmatch (pattern, string, flags) == FNM_NOMATCH;
              }
            ]],
            [[char const *Apat = 'A' < '\\\\' ? "[A-\\\\\\\\]" : "[\\\\\\\\-A]";
              char const *apat = 'a' < '\\\\' ? "[a-\\\\\\\\]" : "[\\\\\\\\-a]";
              static char const A_1[] = { 'A' - 1, 0 };
              static char const A01[] = { 'A' + 1, 0 };
              static char const a_1[] = { 'a' - 1, 0 };
              static char const a01[] = { 'a' + 1, 0 };
              static char const bs_1[] = { '\\\\' - 1, 0 };
              static char const bs01[] = { '\\\\' + 1, 0 };
              int result = 0;
              if (!n ("a*", "", 0))
                return 1;
              if (!y ("a*", "abc", 0))
                return 1;
              if (!y ("[/b", "[/b", 0)) /*"]]"*/ /* glibc Bugzilla bug 12378 */
                return 1;
              if (!n ("d*/*1", "d/s/1", FNM_PATHNAME))
                return 2;
              if (!y ("a\\\\bc", "abc", 0))
                return 3;
              if (!n ("a\\\\bc", "abc", FNM_NOESCAPE))
                return 3;
              if (!y ("*x", ".x", 0))
                return 4;
              if (!n ("*x", ".x", FNM_PERIOD))
                return 4;
              if (!y (Apat, "\\\\", 0))
                return 5;
              if (!y (Apat, "A", 0))
                return 5;
              if (!y (apat, "\\\\", 0))
                return 5;
              if (!y (apat, "a", 0))
                return 5;
              if (!(n (Apat, A_1, 0) == ('A' < '\\\\')))
                return 5;
              if (!(n (apat, a_1, 0) == ('a' < '\\\\')))
                return 5;
              if (!(y (Apat, A01, 0) == ('A' < '\\\\')))
                return 5;
              if (!(y (apat, a01, 0) == ('a' < '\\\\')))
                return 5;
              if (!(y (Apat, bs_1, 0) == ('A' < '\\\\')))
                return 5;
              if (!(y (apat, bs_1, 0) == ('a' < '\\\\')))
                return 5;
              if (!(n (Apat, bs01, 0) == ('A' < '\\\\')))
                return 5;
              if (!(n (apat, bs01, 0) == ('a' < '\\\\')))
                return 5;
              $gl_fnmatch_gnu_start
              if (!y ("xxXX", "xXxX", FNM_CASEFOLD))
                result |= 8;
              if (!y ("a++(x|yy)b", "a+xyyyyxb", FNM_EXTMATCH))
                result |= 16;
              if (!n ("d*/*1", "d/s/1", FNM_FILE_NAME))
                result |= 32;
              if (!y ("*", "x", FNM_FILE_NAME | FNM_LEADING_DIR))
                result |= 64;
              if (!y ("x*", "x/y/z", FNM_FILE_NAME | FNM_LEADING_DIR))
                result |= 64;
              if (!y ("*c*", "c/x", FNM_FILE_NAME | FNM_LEADING_DIR))
                result |= 64;
              $gl_fnmatch_gnu_end
              return result;
            ]])],
         [eval "$gl_fnmatch_cache_var=yes"],
         [eval "$gl_fnmatch_cache_var=no"],
         [case "$host_os" in
                     # Guess yes on musl systems.
            *-musl*) eval "$gl_fnmatch_cache_var=\"guessing yes\"" ;;
                     # Guess no otherwise, even on glibc systems.
            *)       eval "$gl_fnmatch_cache_var=\"guessing no\"" ;;
          esac
         ])
      ])
    eval "gl_fnmatch_result=\"\$$gl_fnmatch_cache_var\""
    case "$gl_fnmatch_result" in
      *yes) ;;
      *) REPLACE_FNMATCH=1 ;;
    esac
  fi
  if test $HAVE_FNMATCH = 0 || test $REPLACE_FNMATCH = 1; then
    gl_REPLACE_FNMATCH_H
  fi
])

# Request a POSIX compliant fnmatch function with GNU extensions.
AC_DEFUN([gl_FUNC_FNMATCH_GNU],
[
  m4_divert_text([INIT_PREPARE], [gl_fnmatch_required=GNU])

  AC_REQUIRE([gl_FUNC_FNMATCH_POSIX])
])

AC_DEFUN([gl_PREREQ_FNMATCH],
[
  dnl Prerequisites of lib/fnmatch.c.
  AC_REQUIRE([AC_TYPE_MBSTATE_T])
  AC_CHECK_FUNCS_ONCE([mbsrtowcs])
])
