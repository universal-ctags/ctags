# ctype_h.m4
# serial 23
dnl Copyright (C) 2009-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

AC_DEFUN_ONCE([gl_CTYPE_H],
[
  AC_REQUIRE([gl_CTYPE_H_DEFAULTS])

  dnl <ctype.h> is always overridden, because of GNULIB_POSIXCHECK.
  gl_NEXT_HEADERS([ctype.h])

  dnl Check for declarations of anything we want to poison if the
  dnl corresponding gnulib module is not in use.
  gl_WARN_ON_USE_PREPARE([[#include <ctype.h>
    ]], [isalnum_l isalpha_l isblank isblank_l iscntrl_l isdigit_l isgraph_l
    islower_l isprint_l ispunct_l isspace_l isupper_l isxdigit_l
    tolower_l toupper_l])
])

# gl_CTYPE_MODULE_INDICATOR([modulename])
# sets the shell variable that indicates the presence of the given module
# to a C preprocessor expression that will evaluate to 1.
# This macro invocation must not occur in macros that are AC_REQUIREd.
AC_DEFUN([gl_CTYPE_MODULE_INDICATOR],
[
  dnl Ensure to expand the default settings once only.
  gl_CTYPE_H_REQUIRE_DEFAULTS
  gl_MODULE_INDICATOR_SET_VARIABLE([$1])
])

# Initializes the default values for AC_SUBSTed shell variables.
# This macro must not be AC_REQUIREd.  It must only be invoked, and only
# outside of macros or in macros that are not AC_REQUIREd.
AC_DEFUN([gl_CTYPE_H_REQUIRE_DEFAULTS],
[
  m4_defun(GL_MODULE_INDICATOR_PREFIX[_CTYPE_H_MODULE_INDICATOR_DEFAULTS], [
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_ISALNUM_L])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_ISALPHA_L])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_ISBLANK])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_ISBLANK_L])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_ISCNTRL_L])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_ISDIGIT_L])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_ISGRAPH_L])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_ISLOWER_L])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_ISPRINT_L])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_ISPUNCT_L])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_ISSPACE_L])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_ISUPPER_L])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_ISXDIGIT_L])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_TOLOWER_L])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_TOUPPER_L])
  ])
  m4_require(GL_MODULE_INDICATOR_PREFIX[_CTYPE_H_MODULE_INDICATOR_DEFAULTS])
  AC_REQUIRE([gl_CTYPE_H_DEFAULTS])
])

AC_DEFUN([gl_CTYPE_H_DEFAULTS],
[
  dnl Assume proper GNU behavior unless another module says otherwise.
  HAVE_ISALNUM_L=1;   AC_SUBST([HAVE_ISALNUM_L])
  HAVE_ISALPHA_L=1;   AC_SUBST([HAVE_ISALPHA_L])
  HAVE_ISBLANK=1;     AC_SUBST([HAVE_ISBLANK])
  HAVE_ISBLANK_L=1;   AC_SUBST([HAVE_ISBLANK_L])
  HAVE_ISCNTRL_L=1;   AC_SUBST([HAVE_ISCNTRL_L])
  HAVE_ISDIGIT_L=1;   AC_SUBST([HAVE_ISDIGIT_L])
  HAVE_ISGRAPH_L=1;   AC_SUBST([HAVE_ISGRAPH_L])
  HAVE_ISLOWER_L=1;   AC_SUBST([HAVE_ISLOWER_L])
  HAVE_ISPRINT_L=1;   AC_SUBST([HAVE_ISPRINT_L])
  HAVE_ISPUNCT_L=1;   AC_SUBST([HAVE_ISPUNCT_L])
  HAVE_ISSPACE_L=1;   AC_SUBST([HAVE_ISSPACE_L])
  HAVE_ISUPPER_L=1;   AC_SUBST([HAVE_ISUPPER_L])
  HAVE_ISXDIGIT_L=1;  AC_SUBST([HAVE_ISXDIGIT_L])
  HAVE_TOLOWER_L=1;   AC_SUBST([HAVE_TOLOWER_L])
  HAVE_TOUPPER_L=1;   AC_SUBST([HAVE_TOUPPER_L])
])
