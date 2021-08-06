# ctype_h.m4 serial 9
dnl Copyright (C) 2009-2021 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN_ONCE([gl_CTYPE_H],
[
  AC_REQUIRE([gl_CTYPE_H_DEFAULTS])

  dnl <ctype.h> is always overridden, because of GNULIB_POSIXCHECK.
  gl_NEXT_HEADERS([ctype.h])

  dnl Check for declarations of anything we want to poison if the
  dnl corresponding gnulib module is not in use.
  gl_WARN_ON_USE_PREPARE([[#include <ctype.h>
    ]], [isblank])
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
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_ISBLANK])
  ])
  m4_require(GL_MODULE_INDICATOR_PREFIX[_CTYPE_H_MODULE_INDICATOR_DEFAULTS])
  AC_REQUIRE([gl_CTYPE_H_DEFAULTS])
])

AC_DEFUN([gl_CTYPE_H_DEFAULTS],
[
  dnl Assume proper GNU behavior unless another module says otherwise.
  HAVE_ISBLANK=1;   AC_SUBST([HAVE_ISBLANK])
])
