# fcntl_h.m4
# serial 22
dnl Copyright (C) 2006-2007, 2009-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.
dnl This file is offered as-is, without any warranty.

# Configure fcntl.h.

dnl Written by Paul Eggert.

AC_DEFUN_ONCE([gl_FCNTL_H],
[
  AC_REQUIRE([gl_FCNTL_H_DEFAULTS])
  AC_REQUIRE([gl_FCNTL_O_FLAGS])
  gl_NEXT_HEADERS([fcntl.h])

  dnl Ensure the type pid_t gets defined.
  AC_REQUIRE([AC_TYPE_PID_T])

  dnl Ensure the type mode_t gets defined.
  AC_REQUIRE([AC_TYPE_MODE_T])

  dnl Check for declarations of anything we want to poison if the
  dnl corresponding gnulib module is not in use, if it is not common
  dnl enough to be declared everywhere.
  gl_WARN_ON_USE_PREPARE([[#include <fcntl.h>
    ]], [fcntl openat openat2])
])

# gl_FCNTL_MODULE_INDICATOR([modulename])
# sets the shell variable that indicates the presence of the given module
# to a C preprocessor expression that will evaluate to 1.
# This macro invocation must not occur in macros that are AC_REQUIREd.
AC_DEFUN([gl_FCNTL_MODULE_INDICATOR],
[
  dnl Ensure to expand the default settings once only.
  gl_FCNTL_H_REQUIRE_DEFAULTS
  gl_MODULE_INDICATOR_SET_VARIABLE([$1])
  dnl Define it also as a C macro, for the benefit of the unit tests.
  gl_MODULE_INDICATOR_FOR_TESTS([$1])
])

# Initializes the default values for AC_SUBSTed shell variables.
# This macro must not be AC_REQUIREd.  It must only be invoked, and only
# outside of macros or in macros that are not AC_REQUIREd.
AC_DEFUN([gl_FCNTL_H_REQUIRE_DEFAULTS],
[
  m4_defun(GL_MODULE_INDICATOR_PREFIX[_FCNTL_H_MODULE_INDICATOR_DEFAULTS], [
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_CREAT])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_FCNTL])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_NONBLOCKING])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_OPEN])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_OPENAT])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_OPENAT2])
    dnl Support Microsoft deprecated alias function names by default.
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_MDA_CREAT], [1])
    gl_MODULE_INDICATOR_INIT_VARIABLE([GNULIB_MDA_OPEN], [1])
  ])
  m4_require(GL_MODULE_INDICATOR_PREFIX[_FCNTL_H_MODULE_INDICATOR_DEFAULTS])
  AC_REQUIRE([gl_FCNTL_H_DEFAULTS])
])

AC_DEFUN([gl_FCNTL_H_DEFAULTS],
[
  dnl Assume proper GNU behavior unless another module says otherwise.
  HAVE_FCNTL=1;          AC_SUBST([HAVE_FCNTL])
  HAVE_OPENAT=1;         AC_SUBST([HAVE_OPENAT])
  HAVE_OPENAT2=1;        AC_SUBST([HAVE_OPENAT2]) dnl GNU/Linux only
  REPLACE_CREAT=0;       AC_SUBST([REPLACE_CREAT])
  REPLACE_FCNTL=0;       AC_SUBST([REPLACE_FCNTL])
  REPLACE_OPEN=0;        AC_SUBST([REPLACE_OPEN])
  REPLACE_OPENAT=0;      AC_SUBST([REPLACE_OPENAT])
])
