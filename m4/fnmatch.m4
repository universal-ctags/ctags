# fnmatch.m4
# serial 21
dnl Copyright (C) 2000-2007, 2009-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

# Check for fnmatch

# Autoconf defines AC_FUNC_FNMATCH, but that is obsolescent.
# New applications should use the macros below instead.

# Request a POSIX compliant fnmatch function.
AC_DEFUN([gl_FUNC_FNMATCH_POSIX],
[
  m4_divert_text([DEFAULTS], [gl_fnmatch_required=POSIX])

  AC_REQUIRE([gl_FNMATCH_H])
  AC_REQUIRE([AC_CANONICAL_HOST])
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
              #include <locale.h>
              #include <stddef.h>
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
              /* ==== Start of tests in the "C" locale ==== */
              /* These are sanity checks. They all succeed on current platforms.  */
              if (!n ("a*", "", 0))
                return 1;
              if (!y ("a*", "abc", 0))
                return 1;
              if (!n ("d*/*1", "d/s/1", FNM_PATHNAME))
                return 1;
              if (!y ("a\\\\bc", "abc", 0))
                return 1;
              if (!n ("a\\\\bc", "abc", FNM_NOESCAPE))
                return 1;
              if (!y ("*x", ".x", 0))
                return 1;
              if (!n ("*x", ".x", FNM_PERIOD))
                return 1;
              /* glibc bug <https://sourceware.org/PR361>
                 exists in glibc 2.3.3, fixed in glibc 2.5.  */
              if (!y (Apat, "\\\\", 0))
                result |= 2;
              if (!y (Apat, "A", 0))
                result |= 2;
              if (!y (apat, "\\\\", 0))
                result |= 2;
              if (!y (apat, "a", 0))
                result |= 2;
              if (!(n (Apat, A_1, 0) == ('A' < '\\\\')))
                result |= 2;
              if (!(n (apat, a_1, 0) == ('a' < '\\\\')))
                result |= 2;
              if (!(y (Apat, A01, 0) == ('A' < '\\\\')))
                result |= 2;
              if (!(y (apat, a01, 0) == ('a' < '\\\\')))
                result |= 2;
              if (!(y (Apat, bs_1, 0) == ('A' < '\\\\')))
                result |= 2;
              if (!(y (apat, bs_1, 0) == ('a' < '\\\\')))
                result |= 2;
              if (!(n (Apat, bs01, 0) == ('A' < '\\\\')))
                result |= 2;
              if (!(n (apat, bs01, 0) == ('a' < '\\\\')))
                result |= 2;
              /* glibc bug <https://sourceware.org/PR12378>
                 exists in glibc 2.12, fixed in glibc 2.13.  */
              if (!y ("[/b", "[/b", 0)) /*"]]"*/
                result |= 4;
              /* glibc bug <https://sourceware.org/PR17062>
                 is fixed in glibc 2.20.
                 glibc bugs <https://sourceware.org/PR18032>
                            <https://sourceware.org/PR18036>
                 are fixed in glibc 2.22.
                 These bugs are not easy to test for reliably (without mmap),
                 therefore test the glibc version.  */
              #if defined __GLIBC__
              if (__GLIBC__ == 2 && __GLIBC_MINOR__ < 22)
                result |= 4;
              #endif
              /* This test fails on FreeBSD 13.2, NetBSD 10.0, Cygwin 3.4.6.  */
              if (!y ("[[:alnum:]]", "a", 0))
                result |= 8;
              $gl_fnmatch_gnu_start /* ==== Start of GNU extensions tests ==== */
              /* Sanity checks, mainly to check the presence of the FNM_* macros.  */
              if (!y ("xxXX", "xXxX", FNM_CASEFOLD))
                result |= 64;
              if (!y ("a++(x|yy)b", "a+xyyyyxb", FNM_EXTMATCH))
                result |= 64;
              if (!n ("d*/*1", "d/s/1", FNM_FILE_NAME))
                result |= 64;
              if (!y ("*", "x", FNM_FILE_NAME | FNM_LEADING_DIR))
                result |= 64;
              if (!y ("x*", "x/y/z", FNM_FILE_NAME | FNM_LEADING_DIR))
                result |= 64;
              if (!y ("*c*", "c/x", FNM_FILE_NAME | FNM_LEADING_DIR))
                result |= 64;
              $gl_fnmatch_gnu_end /* ==== End of GNU extensions tests ==== */
              /* ==== End of tests in the "C" locale ==== */
              /* ==== Start of tests that require a specific locale ==== */
              /* This test fails on Solaris 11.4.  */
              if (setlocale (LC_ALL, "en_US.UTF-8") != NULL)
                {
                  if (!n ("[!a-z]", "", 0))
                    result |= 16;
                }
              /* This test fails on NetBSD 10.0, Android 13.  */
              if (setlocale (LC_ALL, "C.UTF-8") != NULL)
                {
                  if (!y ("x?y", "x\\303\\274y", 0))
                    result |= 32;
                }
              /* ==== End of tests that require a specific locale ==== */
              return result;
            ]])],
         [eval "$gl_fnmatch_cache_var=yes"],
         [eval "$gl_fnmatch_cache_var=no"],
         [case "$host_os" in
                                # Guess yes on musl systems.
            *-musl* | midipix*) eval "$gl_fnmatch_cache_var=\"guessing yes\"" ;;
                                # Guess no otherwise, even on glibc systems.
            *)                  eval "$gl_fnmatch_cache_var=\"guessing no\"" ;;
          esac
         ])
      ])
    eval "gl_fnmatch_result=\"\$$gl_fnmatch_cache_var\""
    case "$gl_fnmatch_result" in
      *yes) ;;
      *) REPLACE_FNMATCH=1 ;;
    esac
    dnl On AIX 7.2 in 32-bit mode, fnmatch()'s only POSIX compliance problem is
    dnl that is does not support characters outside the Unicode BMP correctly.
    dnl Test case: fnmatch ("x?y", "x\360\237\230\213y", 0) == 0
    dnl This is due to wchar_t being only 16 bits wide.
    AC_REQUIRE([gl_UCHAR_H])
    if test $SMALL_WCHAR_T = 1; then
      case "$host_os" in
        cygwin*)
          dnl On Cygwin < 3.5.0, the above $gl_fnmatch_result came out as 'no',
          dnl On Cygwin >= 3.5.0, fnmatch supports all Unicode characters,
          dnl despite wchar_t being only 16 bits wide (because internally it
          dnl works on wint_t values).
          ;;
        *)
          REPLACE_FNMATCH=1
          ;;
      esac
    fi
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
