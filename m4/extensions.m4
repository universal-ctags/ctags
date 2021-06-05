# serial 22  -*- Autoconf -*-
# Enable extensions on systems that normally disable them.

# Copyright (C) 2003, 2006-2021 Free Software Foundation, Inc.
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

dnl Define to empty for the benefit of Autoconf 2.69 and earlier, so that
dnl AC_USE_SYSTEM_EXTENSIONS (below) can be used unchanged from Autoconf 2.70+.
m4_ifndef([AC_CHECK_INCLUDES_DEFAULT],
  [AC_DEFUN([AC_CHECK_INCLUDES_DEFAULT], [])])

# This definition of AC_USE_SYSTEM_EXTENSIONS is stolen from git
# Autoconf.  Perhaps we can remove this once we can assume Autoconf
# is recent-enough everywhere, but since Autoconf mutates rapidly
# enough in this area it's likely we'll need to redefine
# AC_USE_SYSTEM_EXTENSIONS for quite some time.

# If autoconf reports a warning
#     warning: AC_COMPILE_IFELSE was called before AC_USE_SYSTEM_EXTENSIONS
# or  warning: AC_RUN_IFELSE was called before AC_USE_SYSTEM_EXTENSIONS
# the fix is
#   1) to ensure that AC_USE_SYSTEM_EXTENSIONS is never directly invoked
#      but always AC_REQUIREd,
#   2) to ensure that for each occurrence of
#        AC_REQUIRE([AC_USE_SYSTEM_EXTENSIONS])
#      or
#        AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])
#      the corresponding gnulib module description has 'extensions' among
#      its dependencies. This will ensure that the gl_USE_SYSTEM_EXTENSIONS
#      invocation occurs in gl_EARLY, not in gl_INIT.

m4_version_prereq([2.70.1], [], [

# AC_USE_SYSTEM_EXTENSIONS
# ------------------------
# Enable extensions on systems that normally disable them,
# typically due to standards-conformance issues.
# We unconditionally define as many of the known feature-enabling
# as possible, reserving conditional behavior for macros that are
# known to cause problems on some platforms (such as __EXTENSIONS__).
AC_DEFUN_ONCE([AC_USE_SYSTEM_EXTENSIONS],
[AC_BEFORE([$0], [AC_PREPROC_IFELSE])dnl
AC_BEFORE([$0], [AC_COMPILE_IFELSE])dnl
AC_BEFORE([$0], [AC_LINK_IFELSE])dnl
AC_BEFORE([$0], [AC_RUN_IFELSE])dnl
AC_BEFORE([$0], [AC_CHECK_INCLUDES_DEFAULT])dnl
dnl #undef in AH_VERBATIM gets replaced with #define by AC_DEFINE.
dnl Use a different key than __EXTENSIONS__, as that name broke existing
dnl configure.ac when using autoheader 2.62.
dnl The macros below are in alphabetical order ignoring leading _ or __
dnl prefixes.
AH_VERBATIM([USE_SYSTEM_EXTENSIONS],
[/* Enable extensions on AIX 3, Interix.  */
#ifndef _ALL_SOURCE
# undef _ALL_SOURCE
#endif
/* Enable general extensions on macOS.  */
#ifndef _DARWIN_C_SOURCE
# undef _DARWIN_C_SOURCE
#endif
/* Enable general extensions on Solaris.  */
#ifndef __EXTENSIONS__
# undef __EXTENSIONS__
#endif
/* Enable GNU extensions on systems that have them.  */
#ifndef _GNU_SOURCE
# undef _GNU_SOURCE
#endif
/* Enable X/Open compliant socket functions that do not require linking
   with -lxnet on HP-UX 11.11.  */
#ifndef _HPUX_ALT_XOPEN_SOCKET_API
# undef _HPUX_ALT_XOPEN_SOCKET_API
#endif
/* Identify the host operating system as Minix.
   This macro does not affect the system headers' behavior.
   A future release of Autoconf may stop defining this macro.  */
#ifndef _MINIX
# undef _MINIX
#endif
/* Enable general extensions on NetBSD.
   Enable NetBSD compatibility extensions on Minix.  */
#ifndef _NETBSD_SOURCE
# undef _NETBSD_SOURCE
#endif
/* Enable OpenBSD compatibility extensions on NetBSD.
   Oddly enough, this does nothing on OpenBSD.  */
#ifndef _OPENBSD_SOURCE
# undef _OPENBSD_SOURCE
#endif
/* Define to 1 if needed for POSIX-compatible behavior.  */
#ifndef _POSIX_SOURCE
# undef _POSIX_SOURCE
#endif
/* Define to 2 if needed for POSIX-compatible behavior.  */
#ifndef _POSIX_1_SOURCE
# undef _POSIX_1_SOURCE
#endif
/* Enable POSIX-compatible threading on Solaris.  */
#ifndef _POSIX_PTHREAD_SEMANTICS
# undef _POSIX_PTHREAD_SEMANTICS
#endif
/* Enable extensions specified by ISO/IEC TS 18661-5:2014.  */
#ifndef __STDC_WANT_IEC_60559_ATTRIBS_EXT__
# undef __STDC_WANT_IEC_60559_ATTRIBS_EXT__
#endif
/* Enable extensions specified by ISO/IEC TS 18661-1:2014.  */
#ifndef __STDC_WANT_IEC_60559_BFP_EXT__
# undef __STDC_WANT_IEC_60559_BFP_EXT__
#endif
/* Enable extensions specified by ISO/IEC TS 18661-2:2015.  */
#ifndef __STDC_WANT_IEC_60559_DFP_EXT__
# undef __STDC_WANT_IEC_60559_DFP_EXT__
#endif
/* Enable extensions specified by ISO/IEC TS 18661-4:2015.  */
#ifndef __STDC_WANT_IEC_60559_FUNCS_EXT__
# undef __STDC_WANT_IEC_60559_FUNCS_EXT__
#endif
/* Enable extensions specified by ISO/IEC TS 18661-3:2015.  */
#ifndef __STDC_WANT_IEC_60559_TYPES_EXT__
# undef __STDC_WANT_IEC_60559_TYPES_EXT__
#endif
/* Enable extensions specified by ISO/IEC TR 24731-2:2010.  */
#ifndef __STDC_WANT_LIB_EXT2__
# undef __STDC_WANT_LIB_EXT2__
#endif
/* Enable extensions specified by ISO/IEC 24747:2009.  */
#ifndef __STDC_WANT_MATH_SPEC_FUNCS__
# undef __STDC_WANT_MATH_SPEC_FUNCS__
#endif
/* Enable extensions on HP NonStop.  */
#ifndef _TANDEM_SOURCE
# undef _TANDEM_SOURCE
#endif
/* Enable X/Open extensions.  Define to 500 only if necessary
   to make mbstate_t available.  */
#ifndef _XOPEN_SOURCE
# undef _XOPEN_SOURCE
#endif
])dnl

  AC_REQUIRE([AC_CHECK_INCLUDES_DEFAULT])dnl
  _AC_CHECK_HEADER_ONCE([wchar.h])
  _AC_CHECK_HEADER_ONCE([minix/config.h])

dnl Defining __EXTENSIONS__ may break the system headers on some systems.
dnl (FIXME: Which ones?)
  AC_CACHE_CHECK([whether it is safe to define __EXTENSIONS__],
    [ac_cv_safe_to_define___extensions__],
    [AC_COMPILE_IFELSE(
       [AC_LANG_PROGRAM([[
#         define __EXTENSIONS__ 1
          ]AC_INCLUDES_DEFAULT])],
       [ac_cv_safe_to_define___extensions__=yes],
       [ac_cv_safe_to_define___extensions__=no])])

dnl HP-UX 11.11 defines mbstate_t only if _XOPEN_SOURCE is defined to
dnl 500, regardless of whether compiling with -Ae or -D_HPUX_SOURCE=1.
dnl But defining _XOPEN_SOURCE may turn *off* extensions on platforms
dnl not covered by turn-on-extensions macros (notably Dragonfly, Free,
dnl and OpenBSD, which don't have any equivalent of _NETBSD_SOURCE) so
dnl it should only be defined when necessary.
  AC_CACHE_CHECK([whether _XOPEN_SOURCE should be defined],
    [ac_cv_should_define__xopen_source],
    [ac_cv_should_define__xopen_source=no
    AS_IF([test $ac_cv_header_wchar_h = yes],
      [AC_COMPILE_IFELSE(
        [AC_LANG_PROGRAM([[
          #include <wchar.h>
          mbstate_t x;]])],
        [],
        [AC_COMPILE_IFELSE(
          [AC_LANG_PROGRAM([[
            #define _XOPEN_SOURCE 500
            #include <wchar.h>
            mbstate_t x;]])],
          [ac_cv_should_define__xopen_source=yes])])])])

  AC_DEFINE([_ALL_SOURCE])
  AC_DEFINE([_DARWIN_C_SOURCE])
  AC_DEFINE([_GNU_SOURCE])
  AC_DEFINE([_HPUX_ALT_XOPEN_SOCKET_API])
  AC_DEFINE([_NETBSD_SOURCE])
  AC_DEFINE([_OPENBSD_SOURCE])
  AC_DEFINE([_POSIX_PTHREAD_SEMANTICS])
  AC_DEFINE([__STDC_WANT_IEC_60559_ATTRIBS_EXT__])
  AC_DEFINE([__STDC_WANT_IEC_60559_BFP_EXT__])
  AC_DEFINE([__STDC_WANT_IEC_60559_DFP_EXT__])
  AC_DEFINE([__STDC_WANT_IEC_60559_FUNCS_EXT__])
  AC_DEFINE([__STDC_WANT_IEC_60559_TYPES_EXT__])
  AC_DEFINE([__STDC_WANT_LIB_EXT2__])
  AC_DEFINE([__STDC_WANT_MATH_SPEC_FUNCS__])
  AC_DEFINE([_TANDEM_SOURCE])
  AS_IF([test $ac_cv_header_minix_config_h = yes],
    [MINIX=yes
    AC_DEFINE([_MINIX])
    AC_DEFINE([_POSIX_SOURCE])
    AC_DEFINE([_POSIX_1_SOURCE], [2])],
    [MINIX=])
  AS_IF([test $ac_cv_safe_to_define___extensions__ = yes],
    [AC_DEFINE([__EXTENSIONS__])])
  AS_IF([test $ac_cv_should_define__xopen_source = yes],
    [AC_DEFINE([_XOPEN_SOURCE], [500])])
])# AC_USE_SYSTEM_EXTENSIONS
])

# gl_USE_SYSTEM_EXTENSIONS
# ------------------------
# Enable extensions on systems that normally disable them,
# typically due to standards-conformance issues.
AC_DEFUN_ONCE([gl_USE_SYSTEM_EXTENSIONS],
[
  AC_REQUIRE([AC_USE_SYSTEM_EXTENSIONS])

  dnl On OpenBSD 6.8 with GCC, the include files contain a couple of
  dnl definitions that are only activated with an explicit -D_ISOC11_SOURCE.
  dnl That's because this version of GCC (4.2.1) supports the option
  dnl '-std=gnu99' but not the option '-std=gnu11'.
  AC_REQUIRE([AC_CANONICAL_HOST])
  case "$host_os" in
    openbsd*)
      AC_DEFINE([_ISOC11_SOURCE], [1],
        [Define to enable the declarations of ISO C 11 types and functions.])
      ;;
  esac
])
