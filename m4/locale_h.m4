# locale_h.m4 serial 28
dnl Copyright (C) 2007, 2009-2021 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN_ONCE([gl_LOCALE_H],
[
  dnl Ensure to expand the default settings once only, before all statements
  dnl that occur in other macros.
  AC_REQUIRE([gl_LOCALE_H_DEFAULTS])

  dnl Persuade glibc <locale.h> to define locale_t and the int_p_*, int_n_*
  dnl members of 'struct lconv'.
  AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])

  dnl If <stddef.h> is replaced, then <locale.h> must also be replaced.
  AC_REQUIRE([gl_STDDEF_H])

  AC_REQUIRE([gl_LOCALE_T])

  dnl Solaris 11.0 defines the int_p_*, int_n_* members of 'struct lconv'
  dnl only if _LCONV_C99 is defined.
  AC_REQUIRE([AC_CANONICAL_HOST])
  case "$host_os" in
    solaris*)
      AC_DEFINE([_LCONV_C99], [1], [Define to 1 on Solaris.])
      ;;
  esac

  AC_CACHE_CHECK([whether locale.h conforms to POSIX:2001],
    [gl_cv_header_locale_h_posix2001],
    [AC_COMPILE_IFELSE(
       [AC_LANG_PROGRAM(
          [[#include <locale.h>
            int x = LC_MESSAGES;
            int y = sizeof (((struct lconv *) 0)->decimal_point);]],
          [[]])],
       [gl_cv_header_locale_h_posix2001=yes],
       [gl_cv_header_locale_h_posix2001=no])])

  dnl Check whether 'struct lconv' is complete.
  dnl Bionic libc's 'struct lconv' is just a dummy.
  dnl On OpenBSD 4.9, HP-UX 11, IRIX 6.5, OSF/1 5.1, Solaris 9, Cygwin 1.5.x,
  dnl mingw, MSVC 9, it lacks the int_p_* and int_n_* members.
  AC_CACHE_CHECK([whether struct lconv is properly defined],
    [gl_cv_sys_struct_lconv_ok],
    [AC_COMPILE_IFELSE(
       [AC_LANG_PROGRAM(
          [[#include <locale.h>
            struct lconv l;
            int x = sizeof (l.decimal_point);
            int y = sizeof (l.int_p_cs_precedes);]],
          [[]])],
       [gl_cv_sys_struct_lconv_ok=yes],
       [gl_cv_sys_struct_lconv_ok=no])
    ])
  if test $gl_cv_sys_struct_lconv_ok = no; then
    dnl On native Windows with MSVC, merely define these member names as macros.
    dnl This avoids trouble in C++ mode.
    case "$host_os" in
      mingw*)
        AC_EGREP_CPP([Special], [
#ifdef _MSC_VER
 Special
#endif
          ],
          [],
          [REPLACE_STRUCT_LCONV=1])
        ;;
      *) REPLACE_STRUCT_LCONV=1 ;;
    esac
  fi

  dnl <locale.h> is always overridden, because of GNULIB_POSIXCHECK.
  gl_NEXT_HEADERS([locale.h])

  dnl Check for declarations of anything we want to poison if the
  dnl corresponding gnulib module is not in use.
  gl_WARN_ON_USE_PREPARE([[#include <locale.h>
/* Some systems provide declarations in a non-standard header.  */
#if HAVE_XLOCALE_H
# include <xlocale.h>
#endif
    ]],
    [setlocale newlocale duplocale freelocale])
])

dnl Checks to determine whether the system has the locale_t type,
dnl and how to obtain it.
AC_DEFUN([gl_LOCALE_T],
[
  dnl Persuade glibc and Solaris <locale.h> to define locale_t.
  AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])

  dnl Check whether use of locale_t requires inclusion of <xlocale.h>,
  dnl e.g. on Mac OS X 10.5. If <locale.h> does not define locale_t by
  dnl itself, we assume that <xlocale.h> will do so.
  AC_CACHE_CHECK([whether locale.h defines locale_t],
    [gl_cv_header_locale_has_locale_t],
    [AC_COMPILE_IFELSE(
       [AC_LANG_PROGRAM(
          [[#include <locale.h>
            locale_t x;]],
          [[]])],
       [gl_cv_header_locale_has_locale_t=yes],
       [gl_cv_header_locale_has_locale_t=no])
    ])

  dnl Check for <xlocale.h>.
  AC_CHECK_HEADERS_ONCE([xlocale.h])
  if test $ac_cv_header_xlocale_h = yes; then
    HAVE_XLOCALE_H=1
    if test $gl_cv_header_locale_has_locale_t = yes; then
      gl_cv_header_locale_h_needs_xlocale_h=no
    else
      gl_cv_header_locale_h_needs_xlocale_h=yes
    fi
    HAVE_LOCALE_T=1
  else
    HAVE_XLOCALE_H=0
    gl_cv_header_locale_h_needs_xlocale_h=no
    if test $gl_cv_header_locale_has_locale_t = yes; then
      HAVE_LOCALE_T=1
    else
      HAVE_LOCALE_T=0
    fi
  fi
  AC_SUBST([HAVE_XLOCALE_H])
])

# gl_LOCALE_MODULE_INDICATOR([modulename])
# sets the shell variable that indicates the presence of the given module
# to a C preprocessor expression that will evaluate to 1.
# This macro invocation must not occur in macros that are AC_REQUIREd.
AC_DEFUN([gl_LOCALE_MODULE_INDICATOR],
[
  dnl Ensure to expand the default settings once only.
  gl_LOCALE_H_REQUIRE_DEFAULTS
  gl_MODULE_INDICATOR_SET_VARIABLE([$1])
  dnl Define it also as a C macro, for the benefit of the unit tests.
  gl_MODULE_INDICATOR_FOR_TESTS([$1])
])

# Initializes the default values for AC_SUBSTed shell variables.
# This macro must not be AC_REQUIREd.  It must only be invoked, and only
# outside of macros or in macros that are not AC_REQUIREd.
AC_DEFUN([gl_LOCALE_H_REQUIRE_DEFAULTS],
[
  m4_defun(GL_MODULE_INDICATOR_PREFIX[_LOCALE_H_MODULE_INDICATOR_DEFAULTS], [
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_LOCALECONV])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_SETLOCALE])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_SETLOCALE_NULL])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_DUPLOCALE])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_LOCALENAME])
  ])
  m4_require(GL_MODULE_INDICATOR_PREFIX[_LOCALE_H_MODULE_INDICATOR_DEFAULTS])
  AC_REQUIRE([gl_LOCALE_H_DEFAULTS])
])

AC_DEFUN([gl_LOCALE_H_DEFAULTS],
[
  dnl Assume proper GNU behavior unless another module says otherwise.
  HAVE_NEWLOCALE=1;       AC_SUBST([HAVE_NEWLOCALE])
  HAVE_DUPLOCALE=1;       AC_SUBST([HAVE_DUPLOCALE])
  HAVE_FREELOCALE=1;      AC_SUBST([HAVE_FREELOCALE])
  REPLACE_LOCALECONV=0;   AC_SUBST([REPLACE_LOCALECONV])
  REPLACE_SETLOCALE=0;    AC_SUBST([REPLACE_SETLOCALE])
  REPLACE_NEWLOCALE=0;    AC_SUBST([REPLACE_NEWLOCALE])
  REPLACE_DUPLOCALE=0;    AC_SUBST([REPLACE_DUPLOCALE])
  REPLACE_FREELOCALE=0;   AC_SUBST([REPLACE_FREELOCALE])
  REPLACE_STRUCT_LCONV=0; AC_SUBST([REPLACE_STRUCT_LCONV])
  LOCALENAME_ENHANCE_LOCALE_FUNCS=0; AC_SUBST([LOCALENAME_ENHANCE_LOCALE_FUNCS])
])
